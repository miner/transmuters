(ns miner.test-transmuters
  (:require [clojure.test :refer :all]
            [miner.transmuters :refer :all]))


(defn test-take-nth [coll]
  (is (= (clojure.core/take-nth 3 coll)
         (my-take-nth 3 coll)
         (sequence (my-take-nth 3) coll)
         (transduce (my-take-nth 3) conj [] coll))))

(defn test-drop-nth [coll]
  (is (= (my-drop-nth 3 coll)
         (sequence (my-drop-nth 3) coll)
         (transduce (my-drop-nth 3) conj [] coll)
         (keep-indexed (fn [i x] (when (pos? (mod i 3)) x)) coll)
         (sequence (keep-indexed (fn [i x] (when (pos? (mod i 3)) x))) coll))))


(defn test-empty [coll]
  (is (empty? (sequence (my-empty) coll)))
  (is (empty? (my-empty coll))))

;; for reference only, this is "wrong" if the first item doesn't satisfy f
;; Don't use this.  slow-partition-when is a fix.
(defn naive-partition-when [f coll]
  (map (partial apply concat) (partition-all 2 (partition-by f coll))))

;; Slow, but useful for testing
(defn slow-partition-when [f coll]
  (let [ps (partition-by f coll)
        ps (cond (empty? ps) ()
                 (when-let [p1 (ffirst ps)] (f p1)) ps
                 :else (conj ps ()))]
  (map (partial apply concat) (partition-all 2 ps))))

(defn test-partition-when [f coll]
  (is (= (slow-partition-when f coll)
         (partition-when f coll)
         (sequence (partition-when f) coll)
         (transduce (partition-when f) conj [] coll))))


(deftest ranges
  (let [coll (range 100)]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-partition-when even? coll)
    (test-partition-when odd? coll)
    (test-empty coll))
  (let [coll [1]]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-partition-when even? coll)
    (test-partition-when odd? coll)
    (test-empty coll))
  (let [coll ()]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-partition-when even? coll)
    (test-partition-when odd? coll)
    (test-empty coll)))

    

  
