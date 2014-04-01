;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turing machine implementation in Clojure 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns turing
  [:use [clojure.test]])

(defn tape 
  "Creates a new tape with given blank character and tape contents"
  ([blank] (tape () blank () blank))
  ([right blank] (tape () (first right) (rest right) blank))
  ([left head right blank] [(reverse left) (or head blank) (into () right) blank]))

; Tape operations
(defn- left  [[[l & ls] _ rs       b] c] [ls          (or l b) (conj rs c) b])
(defn- right [[ls       _ [r & rs] b] c] [(conj ls c) (or r b) rs          b])
(defn- stay  [[ls       _ rs       b] c] [ls          c        rs          b])
(defn- head [[_ c _ b]] (or c b))
(defn- pretty [[ls c rs b]] (concat (reverse ls) [[(or c b)]] rs))

(defn new-machine 
  "Returns a function that takes a tape as input, and returns the tape
  after running the machine specified in `machine`."
  [machine]
  (let [rules (into {} (for [[s c c' a s'] (:rules machine)]
                         [[s c] [c' (-> a name symbol resolve) s']]))
        finished? (into #{} (:terminating machine))]
    (fn [input-tape]
      (loop [state (:initial machine) tape input-tape]
        (if (finished? state)
          (pretty tape)
          (let [[out action new-state] (get rules [state (head tape)])]
            (recur new-state (action tape out))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def simple-incrementer
  (new-machine {:initial :q0
                :terminating [:qf]
                :rules [[:q0 1   1 :right :q0]
                        [:q0 \B  1 :stay  :qf]]}))
(deftest simple-incrementer-test
  (is (= [1 1 1 [1]] (simple-incrementer (tape [1 1 1] \B)))))


(def three-state-two-symbol-busy-beaver
  (new-machine {:initial :a
                :terminating [:halt]
                :rules [[:a 0  1 :right :b]
                        [:a 1  1 :left  :c]
                        [:b 0  1 :left  :a]
                        [:b 1  1 :right :b]
                        [:c 0  1 :left  :b]
                        [:c 1  1 :stay  :halt]]}))
(deftest three-state-two-symbol-busy-beaver-test
  (is (= [1 1 1 [1] 1 1] (three-state-two-symbol-busy-beaver (tape 0)))))


(def five-state-two-symbol-busy-beaver
  (new-machine {:initial :A
                :terminating [:H]
                :rules [[:A 0  1 :right :B]
                        [:A 1  1 :left  :C]
                        [:B 0  1 :right :C]
                        [:B 1  1 :right :B]
                        [:C 0  1 :right :D]
                        [:C 1  0 :left  :E]
                        [:D 0  1 :left  :A]
                        [:D 1  1 :left  :D]
                        [:E 0  1 :stay  :H]
                        [:E 1  0 :left  :A]]}))
(deftest five-state-two-symbol-busy-beaver-test
  (let [result (flatten (five-state-two-symbol-busy-beaver (tape 0)))
        freq (frequencies result)]
    (is (= 4098 (get freq 1)))
    (is (= 8191 (get freq 0)))))
