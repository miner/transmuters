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


(deftest ranges
  (let [coll (range 100)]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-empty coll))
  (let [coll [1]]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-empty coll))
  (let [coll ()]
    (test-take-nth coll)
    (test-drop-nth coll)
    (test-empty coll)))

    

  
