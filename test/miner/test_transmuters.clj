(ns miner.test-transmuters
  (:require [clojure.test :refer :all]
            [miner.transmuters :refer :all]))


(defn test-take-nth [coll]
  (is (= (clojure.core/take-nth 3 coll)
         (my-take-nth 3 coll)
         (sequence (my-take-nth 3) coll)
         (transduce (my-take-nth 3) conj [] coll))))

(defn test-drop-nth [coll]
  (is (= (drop-nth 3 coll)
         (sequence (drop-nth 3) coll)
         (transduce (drop-nth 3) conj [] coll)
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
  (doseq [coll (list () [1] (range 100))]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-partition-when even? coll)
    (test-partition-when odd? coll)
    (test-empty coll)))


(deftest part-while
  (is (= (partition-while >= [1 2 2 3 1 5 3])
         '([1] [2 2] [3 1] [5 3])))
  (is (= (partition-while > [1 2 2 3 1 5 3])
         '([1] [2] [2] [3 1] [5 3])))
  (is (= (partition-while <= [1 2 2 3 1 5 3])
         '([1 2 2 3] [1 5] [3])))
  (is (= (partition-while < [1 2 2 3 1 5 3])
         '([1 2] [2 3] [1 5] [3]))))


                                             
(deftest thresholds
  (let [coll (range 100)]
    (is (= (sequence (partition-threshold (constantly 1) 13) coll)
           (sequence (partition-all 13) coll)))
    (is (= (sequence (comp (partition-all 4) (partition-threshold (app max) 100)
                           (map flatten) (map (app +)))
                     (range 100))
           '(378 402 546 444 508 572 636 700 764)))
    (is (= (sequence (partition-threshold identity 81) coll)
           '([0 1 2 3 4 5 6 7 8 9 10 11 12 13] [14 15 16 17 18 19] [20 21 22 23]
            [24 25 26 27] [28 29 30] [31 32 33] [34 35 36] [37 38 39] [40 41] [42 43]
            [44 45] [46 47] [48 49] [50 51] [52 53] [54 55] [56 57] [58 59] [60 61]
            [62 63] [64 65] [66 67] [68 69] [70 71] [72 73] [74 75] [76 77] [78 79]
            [80 81] [82] [83] [84] [85] [86] [87] [88] [89] [90] [91] [92] [93] [94]
            [95] [96] [97] [98] [99])))))

(defn sq [n] (* n n))

(deftest chaining
  (let [coll (range 100)]
    (is (= (sequence (chain) coll) coll))
    (is (= (sequence (chain (comp)) coll) coll))
    (is (= (sequence (chain (map sq)) coll) (map sq coll)))
    ;; (take 0) is a special case that burns on input so we need the pushback
    (is (= (sequence (chain (take 0) pushback (map sq)) coll) (map sq coll)))
    (is (= (sequence (chain (take 5)) coll) (take 5 coll)))
    (is (= (sequence (chain (comp (drop-while #(< % 4)) (take 3)) (map -)) coll)
           (sequence (chain (comp (drop 4) (take-while #(< % 7))) pushback (map -)) coll)
           (concat (take 3 (drop 4 coll)) (map - (drop 7 coll)))))
    (is (= (sequence (chain (comp (take 5) (map sq)) (map -)) coll) 
           (concat (map sq (take 5 coll)) (map - (drop 5 coll)))))
    (is (= (sequence (comp (map (partial + 10)) (chain (comp (take 5) (map sq)) (map -)) ) coll)
           (let [coll10 (map (partial + 10) coll)]
             (concat (map sq (take 5 coll10)) (map - (drop 5 coll10))))))
    ;; take-while burns an extra input so you need to pushback
    (is (= (sequence (chain (comp (take 5) (map sq))
                            (comp (take-while #(< % 20)) (map -)) pushback
                            (comp (take 10) (map sq))
                            (filter even?))
                     coll)
           (concat (map sq (take 5 coll))
                   (map - (take 15 (drop 5 coll)))
                   (map sq (take 10 (drop 20 coll)))
                   (filter even? (drop 30 coll)))))))

(deftest completion-chain
  (is (= (sequence (chain (comp (take 7) (partition-all 6)) (partition-all 3)) (range 20))
         '([0 1 2 3 4 5] [6] [7 8 9] [10 11 12] [13 14 15] [16 17 18] [19]))))

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
        
(deftest while-accumulating
  ;; distinct
  (is (= (into [] (take-while2 conj #{} (complement contains?)) '(1 2 3 4 2 5 6))
         [1 2 3 4]))
  ;; dedupe
  (is (= (into [] (take-while2 (farg 2) ::void not=) '(1 2 1 3 4 4 5 6))
         [1 2 1 3 4]))
  ;; monotonically increasing
  (is (= (into [] (take-while2 max 0 <=) '(1 2 3 4 4 1 5 6))
         [1 2 3 4 4])))


(deftest sliding
  (is (= (sequence (comp (slide 3 [0 0 0]) (map (app +))) (range 10))
         '(0 1 3 6 9 12 15 18 21 24)))
  (is (= (sequence (comp (slide 4) (map vec)) (range 10))
         '([0 1 2 3] [1 2 3 4] [2 3 4 5] [3 4 5 6] [4 5 6 7] [5 6 7 8] [6 7 8 9]))))

(deftest convolutions
  (is (= (sequence (convolve [0.25 0.5 0.25]) [1 2 3 4 5])
         '(2.0 3.0 4.0))))

(defn limit10+
  ([] 0)
  ([a b] (let [sum (+ a b)] (if (> sum 10) (reduced sum) sum))))

(deftest xreducts
  (is (= (reductions + 3 (range 20)) (sequence (reducts + 3) (range 20))))
  (is (= (reductions limit10+ 3 (range 20)) (sequence (reducts limit10+ 3) (range 20)))))

(deftest prepend-append
  (is (= (sequence (prepend [3 4 5]) ()) '(3 4 5)))
  (is (= (sequence (comp (prepend [3 4]) (map #(* 2 %))) (range 4))
         '(6 8 0 2 4 6)))
  (is (= (sequence (comp (map #(* 2 %)) (prepend [3 4]) ) (range 4))
         '(3 4 0 2 4 6)))
  (is (= (sequence (prepend [11 12]) (range 10)) (concat [11 12] (range 10))))
  (is (= (sequence (append [11 12]) (range 10)) (concat (range 10) [11 12])))
  (is (= (sequence (comp (prepend [4]) (reducts limit10+ 3) (append [101])) (range 10))
         '(3 7 7 8 10 13 101))))

#_
(deftest map-cycling
  (is (= (sequence (map-cycle + - + + -) (range 20))
         '(0 -1 2 3 -4 5 -6 7 8 -9 10 -11 12 13 -14 15 -16 17 18 -19)))
  (is (= (sequence (map-cycle +) (range 20)) (range 20))))

