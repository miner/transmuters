(ns miner.transmuters)


;; empty would make a convenient transducer that does nothing (ultimately returns empty list)
(defn my-empty
  "Like empty, but no-arg yields degenerate transducer that alway returns empty list"
  ([] (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result)))))
  ([coll] (empty coll)))
  

;; I think (take-nth 0) should always be the empty list
(defn my-take-nth
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided.  N=0 returns empty list."
  {:added "1.0"
   :static true}
  ([n]
   (if (zero? n)
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result))))
     (fn [rf]
       (let [iv (volatile! 1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv dec)]
                (if (zero? i)
                  (do (vreset! iv n)
                      (rf result input))
                  result))))))))
  ([n coll]
   (if (zero? n)
     ()
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (first s) (my-take-nth n (drop n s))))))))

;; will be private
(defn lazy-drop-nth [n coll]
  ;; assumes (pos? n)
  ;; Notice drops first (aiming to be complement of take-nth)
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (take (dec n) (rest s)) (lazy-drop-nth n (drop n s))))))

(defn my-drop-nth
  "Returns a lazy seq dropping the first and every nth item thereafter in coll.  Returns a stateful
  transducer when no collection is provided.  N=0 returns the coll."
  {:static true}
  ([n]
   (if (zero? n)
     conj
     (fn [rf]
       (let [iv (volatile! 1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv dec)]
                (if (zero? i)
                  (do (vreset! iv n)
                      result)
                  (rf result input)))))))))
  ([n coll]
   (if (zero? n)
     coll
     (lazy-drop-nth n coll))))
