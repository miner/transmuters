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
  (is (empty? (sequence (xempty) coll)))
  (is (empty? (xempty coll))))

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

    

  
(deftest accumulations
  (let [coll (range 100)]
    (is (= (sequence (accumulate (partial = 13) (fn [acc _] (inc acc)) 0) coll)
           (accumulate (partial = 13) (fn [acc _] (inc acc)) 0 coll)
           (sequence (partition-all 13) coll)))
    (is (= (sequence (accumulate (fn [x] (> x 80)) + 0) coll)
           (accumulate (fn [x] (> x 80)) + 0 coll)
           '([0 1 2 3 4 5 6 7 8 9 10 11 12 13] [14 15 16 17 18 19] [20 21 22 23]
            [24 25 26 27] [28 29 30] [31 32 33] [34 35 36] [37 38 39] [40 41] [42 43]
            [44 45] [46 47] [48 49] [50 51] [52 53] [54 55] [56 57] [58 59] [60 61]
            [62 63] [64 65] [66 67] [68 69] [70 71] [72 73] [74 75] [76 77] [78 79]
            [80 81] [82] [83] [84] [85] [86] [87] [88] [89] [90] [91] [92] [93] [94]
            [95] [96] [97] [98] [99])))))

;; handy combinator for testing
;; rem is slightly faster than mod
(defn zmod [n]
  (fn [x] (zero? (rem x n))))

(deftest zzzmods
  (let [coll (range 100)]
    (is (= (range 0 (count coll) 3)
           (filter (zmod 3) coll)
           (sequence (filter (zmod 3)) coll)
           (sequence (take-nth 3) coll)))))
        
