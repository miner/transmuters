(ns miner.test-transmuters
  (:require [clojure.test :refer :all]
            [miner.transmuters :refer :all]))

(deftest range100
  (let [coll (range 100)]
    (is (= (clojure.core/take-nth 3 coll)
           (my-take-nth 3 coll)
           (sequence (my-take-nth 3) coll)
           (transduce (my-take-nth 3) conj [] coll)))
    (is (empty? (sequence (my-empty) coll)))
    (is (empty? (my-empty coll)))))

    

  
