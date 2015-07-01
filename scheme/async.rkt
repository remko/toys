#lang racket/base

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; async
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (async? p)
  (procedure? p))

(define (make-async task)
  (λ (cont)
    (let ([resolve (λ (value) (cont #f value))]
          [reject (λ (reason) (cont reason #f))])
      (task resolve reject))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; async monad methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (async-return value)
  (make-async 
   (λ (resolve reject) (resolve value))))

(define (async-bind async f)
  (make-async 
   (λ (resolve reject)
     (async (λ (async-error async-value)
              (if async-error
                  (reject async-error)
                  ((f async-value) (λ (f-error f-value) 
                                     (if f-error 
                                         (reject f-error)
                                         (resolve f-error))))))))))


(define-syntax async-do
  (syntax-rules ()
    [(_ e ...) (monad-do (async-return async-bind async?) e ...)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List monad methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-bind xs f)
  (append* (map f xs)))

(define-syntax list-do
  (syntax-rules ()
    [(_ e ...) (monad-do (list list-bind list?) e ...)]))

(define (where x)
  (if x (list (void)) '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Maybe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct maybe (value? value) #transparent)

(define (just value) 
  (maybe #t value))

(define (nothing)
  (maybe #f #f))

(define (nothing? m)
  (not (maybe-value? m)))

(define (maybe-bind m f)
  (if (maybe-value? m)
      (f (maybe-value m))
      (nothing)))

(define-syntax maybe-do
  (syntax-rules ()
    [(_ e ...) (monad-do (just maybe-bind maybe?) e ...)]))

(define (->maybe v)
  (if (maybe? v) v (just v)))

(define (?+ ?x ?y)
  (maybe-do
   (<- x ?x)
   (<- y ?y)
   (+ x y)))

(define (?/ ?x ?y)
  (maybe-do
   (<- x ?x)
   (<- y ?y)
   (if (= y 0) 
       (nothing)
       (/ x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Result
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct result (error value))

(define (error-result reason) 
  (result reason #f))

(define (value-result value)
  (result #f value))

(define (error-result? r)
  (result-error r))

(define (value-result? r)
  (not (error-result? r)))

(define (result-bind r f)
  (if (value-result? r)
      (f (result-value r))
      r))

(define-syntax result-do
  (syntax-rules ()
    [(_ e ...) (monad-do (value-result result-bind result?) e ...)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exit-main-loop? #f)

(define (main-loop)
  (if exit-main-loop?
      (void)
      (begin
        ((thread-receive))
        (main-loop))))

(define (reset-main-loop)
  (if (thread-try-receive)
      (reset-main-loop)
      (set! exit-main-loop? #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; async-do*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax async-do*
  (syntax-rules (<-)
    [(_ e) e]
    [(_ (<- var e1) e2 ...)
     (async-bind e1 (λ (var) (async-do* e2 ...)))]
    [(_ e1 e2 ...) 
     (async-bind e1 (λ (_) (async-do* e2 ...)))]))

; Convert a value to an async if it isn't one
(define-syntax ->async
  (syntax-rules ()
    [(_ e) 
     (let ([e-result e])
       (if (async? e-result)
           e-result
           (async-return e-result)))]))

(define-syntax async-do**
  (syntax-rules (<-)
    [(_ e)
     (->async e)]
    [(_ (<- var e1) e2 ...)
     (async-bind (->async e1) (λ (var) (async-do** e2 ...)))]
    [(_ e1 e2 ...)
     (async-bind (->async e1) (λ (_) (async-do** e2 ...)))]))


(define-syntax ->monad
  (syntax-rules ()
    [(_ (return monad?) e) 
     (let ([e-result e])
       (if (monad? e-result)
           e-result
           (return e-result)))]))

(define-syntax monad-do
  (syntax-rules (<-)
    [(_ (return >>= monad?) e)
     (->monad (return monad?) e)]
    [(_ (return >>= monad?) (<- var e1) e2 ...)
     (>>= (->monad (return monad?) e1) 
          (λ (var) (monad-do (return >>= monad?) e2 ...)))]
    [(_ (return >>= monad?) e1 e2 ...)
     (>>= (->monad (return monad?) e1) 
          (λ (_) (monad-do (return >>= monad?) e2 ...)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fetch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fetch key)
  (let ([main-thread (current-thread)])
    (make-async (λ (resolve reject)
                  (thread 
                   (λ ()
                     (sleep .1)
                     (thread-send main-thread
                                  (λ () (resolve (string-append key "-value"))))))))))

(define (fetch-fail key)
  (let ([main-thread (current-thread)])
    (make-async (λ (resolve reject)
                  (thread 
                   (λ ()
                     (sleep .1)
                     (thread-send main-thread
                                  (λ () (reject (string-append "Failed to fetch " key))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require rackunit/text-ui)
(require racket/trace)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; async example tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define do-log '())

(define (do-something-first)
  (set! do-log (cons 'do-something-first do-log)))

(define (do-something-with . values)
  (set! do-log (cons (cons 'do-something-with values) do-log)))

(define (do-something-else)
  (set! do-log (cons 'do-something-else do-log)))

(define (main-loop-with-do-log)
  (if exit-main-loop?
      (void)
      (begin
        (set! do-log (cons '> do-log))
        ((thread-receive))
        (main-loop-with-do-log))))

(define (run-async async)
  (async (λ (err _) 
           (if err 
               (error err) 
               (void))
           (set! exit-main-loop? #t))))

(define (reset-async-test)
  (reset-main-loop)
  (set! do-log '()))

(define async-tests
  (test-suite
   "async Tests"
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "Pyramid Example"
    
    (reset-async-test)
    
    (do-something-first)
    ((fetch "foo") (λ (_ foo)
                     (do-something-with foo)
                     ((fetch "bar") (λ (_ bar)
                                      ; Don't actually need the value of bar
                                      (do-something-else)
                                      ((fetch "baz") (λ (_ baz)
                                                       (do-something-with foo baz)
                                                       (set! exit-main-loop? #t)))))))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(do-something-first
                    > (do-something-with "foo-value")
                    > do-something-else
                    > (do-something-with "foo-value" "baz-value"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "Bind Example"
    
    (reset-async-test)
    
    (run-async 
     (async-bind (fetch "foo")
                 (λ (foo)
                   (async-bind (fetch "bar")
                               (λ (bar)
                                 (do-something-with foo bar)
                                 (async-return '()))))))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(> > (do-something-with "foo-value" "bar-value"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "async-do* Example 1"
    
    (reset-async-test)
    
    (run-async 
     (async-do*
      (<- foo (fetch "foo"))
      (<- bar (fetch "bar"))
      (async-return (do-something-with foo bar))))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(> > (do-something-with "foo-value" "bar-value"))))
   
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "async-do* Example 2"
    
    (reset-async-test)
    
    (run-async 
     (async-do*
      (async-return (do-something-first))
      (<- foo (fetch "foo"))
      (async-return (do-something-with foo))
      (<- bar (fetch "bar"))
      (async-return (do-something-else))
      (<- baz (fetch "baz"))
      (async-return (do-something-with foo baz))))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(do-something-first
                    > (do-something-with "foo-value")
                    > do-something-else
                    > (do-something-with "foo-value" "baz-value"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "async-do** Example"
    
    (reset-async-test)
    
    (run-async 
     (async-do**
      (do-something-first)
      (<- foo (fetch "foo"))
      (do-something-with foo)
      (<- bar (fetch "bar"))
      (do-something-else)
      (<- baz (fetch "baz"))
      (do-something-with foo baz)))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(do-something-first
                    > (do-something-with "foo-value")
                    > do-something-else
                    > (do-something-with "foo-value" "baz-value"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "monad-do Example"
    
    (reset-async-test)
    
    (run-async 
     (monad-do (async-return async-bind async?)
               (do-something-first)
               (<- foo (fetch "foo"))
               (do-something-with foo)
               (<- bar (fetch "bar"))
               (do-something-else)
               (<- baz (fetch "baz"))
               (do-something-with foo baz)))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(do-something-first
                    > (do-something-with "foo-value")
                    > do-something-else
                    > (do-something-with "foo-value" "baz-value"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "async-do Example"
    
    (reset-async-test)
    
    (run-async 
     (async-do
      (do-something-first)
      (<- foo (fetch "foo"))
      (do-something-with foo)
      (<- bar (fetch "bar"))
      (do-something-else)
      (<- baz (fetch "baz"))
      (do-something-with foo baz)))
    
    (main-loop-with-do-log)
    (check-equal? (reverse do-log)
                  '(do-something-first
                    > (do-something-with "foo-value")
                    > do-something-else
                    > (do-something-with "foo-value" "baz-value"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case
    "failing async-do Example"
    
    (reset-async-test)
    
    (run-async 
     (async-do
      (do-something-first)
      (<- foo (fetch "foo"))
      (do-something-with foo)
      (<- bar (fetch-fail "bar"))
      (do-something-else)
      (<- baz (fetch "baz"))
      (do-something-with foo baz)))
    
    (check-exn exn:fail? (λ () (main-loop-with-do-log)))
    (check-equal? (reverse do-log)
                  '(do-something-first
                    > (do-something-with "foo-value")
                    > )))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; List monad example tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-tests
  (test-suite
   "List Tests"
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case 
    "monad-do test"
    
    (define my-pair-list
      (monad-do (list list-bind list?)
                (<- n '(1 2))
                (<- ch '("a" "b"))
                (cons n ch)))
    
    (check-equal? my-pair-list '((1 . "a") (1 . "b") (2 . "a") (2 . "b"))))
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case 
    "list-do test"
    
    (define my-pair-list
      (list-do
       (<- n '(1 2))
       (<- ch '("a" "b"))
       (cons n ch)))
    
    (check-equal? my-pair-list '((1 . "a") (1 . "b") (2 . "a") (2 . "b"))))
   
   (test-case 
    "list-do with guard test"
    
    (define my-pair-list
      (list-do
       (<- x (range 0 4))
       (<- y (range 0 4))
       (where (< x y))
       (cons x y)))
    
    (check-equal? my-pair-list '((0 . 1) (0 . 2) (0 . 3) 
                                         (1 . 2) (1 . 3) 
                                                 (2 . 3))))))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Maybe monad example tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define maybe-tests
  (test-suite
   "Maybe Tests"
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case 
    "maybe-do test"
    
    (define (compute x y z)
       (?+ (?/ x y) z))
    
    (check-pred nothing? (compute 3 0 1))
    (check-eq? (maybe-value (compute 4 2 1)) 3)
    
    ; Auto-lifting
    (check-pred nothing? (?+ 3 (?/ 4 0)))
    (check-eq? (maybe-value (?+ 3 (?/ 4 2))) 5))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Result monad example tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define result-tests
  (test-suite
   "Result Tests"
   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   (test-case 
    "result-do test"
    
    (define (do-something)
      (display "do-something\n")
      (set! do-log (cons 'do-something do-log))
      (value-result 1))
    
    (define (do-something-else . x)
      (display "do-something-else\n")
      (set! do-log (cons (cons 'do-something-else x) do-log))
      (value-result 2))
    
    (define (do-something-that-fails)
      (set! do-log (cons 'do-something-that-fails do-log))
      (error-result "Failed for some reason"))
    
    (define (do-something-complex)
      (result-do
       (<- x (do-something))
       (do-something-else x)))
    
    (define (do-something-complex-that-fails)
      (result-do
       (<- x (do-something))
       (<- y (do-something-that-fails))
       (do-something-else x)))
    
    (set! do-log '())
    (let ([result (do-something-complex)])
      (check-pred value-result? result)
      (check-equal? (result-value result) 2)
      (check-equal? (reverse do-log) '(do-something (do-something-else 1))))
    
    (set! do-log '())
    (let ([result (do-something-complex-that-fails)])
      (check-pred error-result? result)
      (check-equal? (reverse do-log) '(do-something do-something-that-fails))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(run-tests (test-suite "All tests" 
                       async-tests 
                       list-tests
                       maybe-tests
                       result-tests))
