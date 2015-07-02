(ns async.core
  [:require [clojure.core.async :as async :refer [chan <!! >!! thread]]]
  (:gen-class))

(def main-queue (chan 10))

  ;; (require '[clojure.core.async :as async :refer :all])
(defn make-async [thunk]
  (fn [cont]
    (let [resolve #(>!! main-queue (fn [] (cont nil %1)))
          reject #(>!! main-queue (fn [] (cont %1 nil)))]
      (thunk resolve reject))))

(defn async-run [async]
  (async (fn [error _]
             (if error 
                 (throw (Error. error))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Promise monad methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn return [value]
  (make-async (fn [resolve reject] (resolve value))))

(defn >>= [async f]
  (make-async 
    (fn [resolve reject]
      (async (fn [async-error async-value]
                  (if async-error
                      (reject async-error)
                      ((f async-value) (fn [f-error f-value] 
                                          (if f-error 
                                              (reject f-error)
                                              (resolve f-error))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Dummy fetch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fetch [key]
  (make-async (fn [resolve reject] 
                  (thread 
                    (Thread/sleep 1000)
                    (resolve (str key "-value"))))))

; Pyramid
(defn example-pyramid []
  ((fetch "foo") (fn [_ foo]
                   (println foo)
                   ((fetch "bar") (fn [_ bar]
                                   (println "Ignoring bar")
                                   ((fetch "baz") (fn [_ baz]
                                                   (println foo " ")
                                                   (println "Finished"))))))))
; Bind
(defn example-bind []
 (async-run 
    (>>= (fetch "foo")
         (fn [foo]
            (>>= (fetch "bar")
                 (fn [bar]
                    (println foo bar)
                    (return '())))))))
                                 

(defmacro do-p [& body]
  (if (empty? body) 
      (throw (Error. "Missing body"))
      (let [[b & bs] body]
        (cond
          (empty? bs) b

          (and (list? b) (= (first b) '<-))
          (let [[_ target-var call] b]
            `(>>= ~call (fn [~target-var] (do-p ~@bs))))

          :else
          `(>>= ~b (fn [~(gensym)] (do-p ~@bs)))))))

(def ->monad [statement]
  (if (fn? statement) statement 
  
(defmacro do-p* [& body]
  (if (empty? body) 
      (throw (Error. "Missing body"))
      (let [[b & bs] body]
        (cond
          (empty? bs) (->monad) b

          (and (list? b) (= (first b) '<-))
          (let [[_ target-var call] b]
            `(>>= (->monad ~call) (fn [~target-var] (do-p ~@bs))))

          :else
          `(>>= (->monad ~b) (fn [~(gensym)] (do-p ~@bs)))))))
      
; Do-p
(defn example-do-p []
 (async-run 
   (do-p
    (return (println "Starting"))
    (<- foo (fetch "foo"))
    (return (println foo))
    (<- bar (fetch "bar"))
    (return (println "Ignoring bar"))
    (<- baz (fetch "baz"))
    (return (println foo baz))
    (return (println "Finished")))))

(defn example-do-p* []
 (async-run 
   (do-p*
    (println "Starting")
    (<- foo (fetch "foo"))
    (println foo)
    (<- bar (fetch "bar"))
    (println "Ignoring bar")
    (<- baz (fetch "baz"))
    (println foo baz)
    (println "Finished"))))

(defn run-example []
  (example-do-p*))

(defn -main [& args]
  (run-example)
  (loop []
    ((<!! main-queue))
    (recur)))
