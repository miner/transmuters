(ns miner.transmuters)


;; xempty would make a convenient degenerate transducer that does nothing (ultimately returns
;; empty list)

;; But beware, it will consume one input in a chain even though nothing results.
(defn xempty
  "Like empty, but no-arg yields degenerate transducer that alway returns empty list"
  ([] (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result)))))
  ([coll] (empty coll)))

;; xidentity is the identity function for transducers, just passes the input through
;; equivalent to (map identity)
(defn xidentity
  ([] (fn [rf]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input] (rf result input)))))
  ([coll] (seq coll)))


;; works but clunky, keep for testing
(defn chain2
  ([] (fn [rf]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input] (rf result input)))))
  ([xf] xf)
  ([xf xg] (fn [rf]
             (let [vflag (volatile! true)
                   frf (xf rf)
                   grf (xg rf)]
               (fn
                 ([] (if @vflag (frf) (grf)))
                 ([result] (if @vflag (frf result) (grf result)))
                 ([result input]
                  (let [res (if @vflag (frf result input) (grf result input))]
                    (if (reduced? res)
                      (do (vreset! vflag false)
                          (unreduced res))
                      res)))))))
  ([xf xg xh] (chain2 xf (chain2 xg xh)))
  ([xf xg xh & xmore] (chain2 xf (apply chain2 xg xh xmore))))


;; Issue with (take 0) always consuming one input
;; seems to work
(defn chain
  ;; degenerate case is simply pass through (essentially, the transducer identity)
  ([] (fn [rf]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input] (rf result input)))))
  ([xf] xf)
  ([xf & xfs] (fn [rf]
                (let [vxfs (volatile! (map #(% rf) (conj xfs xf)))]
                  (fn
                    ([] (if-let [xform (first @vxfs)] (xform) (rf)))
                    ([result] (if-let [xform (first @vxfs)] (xform result) (rf result)))
                    ([result input]
                     (if-let [xform (first @vxfs)]
                       (let [res (xform result input)]
                         ;; reduced means that xform is done
                         (if (reduced? res)
                           ;; mutating like a C hacker!
                           (if (vswap! vxfs next)
                             (unreduced res)
                             res)
                           res))
                       (ensure-reduced result))))))))




;; I think (take-nth 0) should always be the empty list.
;; This is a change from the way it works in Clojure 1.6.
;; Clojure 1.7.0-alpha5 throws on (take-nth 0), mine gives a transducer that will return ().
;; See also http://dev.clojure.org/jira/browse/CLJ-1665
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


;; collection version by Frank on mailing list
;; SEM added transducer version, adapted from partition-by
(defn partition-when
   "Applies f to each value in coll, starting a new partition each time f returns a
   true value.  Returns a lazy seq of partitions.  Returns a stateful
   transducer when no collection is provided."
  {:added "1.X"
   :static true}
  ([f]
   (fn [rf]
     (let [a (java.util.ArrayList.)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
            (if (.isEmpty a)
              (do (.add a input)
                  result)
              (if (f input)
                (let [v (vec (.toArray a))]
                  (.clear a)
                  (let [ret (rf result v)]
                    (when-not (reduced? ret)
                      (.add a input))
                    ret))
                (do
                  (.add a input)
                  result))))))))

  ([f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           run (cons fst (take-while #(not (f %)) (next s)))]
       (cons run (partition-when f (seq (drop (count run) s)))))))))


;; SEM: accumalate always takes the item that triggers the pred into the current partition.
;; What if you want a stronger guarantee (such as total less than a limit)?  Dual of the
;; issue about how to handle first item if pred decides item should go into new partition.

;; Sort of a cross between partition-all and reduce.  Always add to current partition, start
;; a new one on pred of acc after addition.

;; alternate name: partition-until

(defn accumulate
  "Accumulates sequential items of <coll> into partitions.  As each item goes
  into the current partition, an internal value is calculated by applying <accf> to previous
  accumulation and the incoming item.  If the resulting accumulation satifies <pred>, the
  current partition is complete and the next item will go into a new partition.  The
  internal accumulation starts as <init> for each partition.Returns a stateful transducer
  when no collection is provided."
  {:added "1.X"
   :static true}
  ([pred accf init]
   (fn [rf]
     (let [a (java.util.ArrayList.)
           vacc (volatile! init)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (vreset! vacc nil)  ;; might help gc???
            (rf result)))
         ([result input]
          (.add a input)
          (let [acc (accf @vacc input)]
            (if (pred acc)
              ;; push old, start new partition
              (let [v (vec (.toArray a))]
                (.clear a)
                (let [ret (rf result v)]
                  (vreset! vacc init)
                  ret))
              ;; continue current partition
              (do
                (vreset! vacc acc)
                result))))))))
  ([pred accf init coll]
   (let [[res part] (reduce (fn [[res part acc] item]
                              (let [part (conj part item)
                                    acc (accf acc item)]
                                (if (pred acc)
                                  [(conj res part) [] init]
                                  [res part acc])))
                            [[] [] init]
                            coll)]
     (if (empty? part)
       res
       (conj res part)))))


;;; SEM NEEDS TESTING FOR TRANSDUCER
(defn partition-threshold
  "Accumulates sequential items of <coll> into partitions according to <scoref> function and
  the <threshold> integer.  As each item goes into the current partition, an integral score
  is calculated by applying the <scoref> function to the incoming item and adding the result
  to the previous score.  If the resulting score is greater than or equal to the
  <threshold>, the current partition is complete and the next item will go into a new
  partition.  The internal score starts as 0 for each partition.  Returns a stateful
  transducer when no collection is provided."
  
  {:added "1.X"
   :static true}
  ([scoref threshold]
   (fn [rf]
     (let [a (java.util.ArrayList.)
           vscore (volatile! 0)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (rf result)))
         ([result input]
          (.add a input)
          (let [score (vswap! vscore + (scoref input))]
            (if (>= score threshold)
              ;; push old, start new partition
              (let [v (vec (.toArray a))]
                (.clear a)
                (let [ret (rf result v)]
                  (vreset! vscore 0)
                  ret))
              ;; continue current partition
              result)))))))
  ([scoref threshold coll]
   (let [[res part] (reduce (fn [[res part score] item]
                              (let [part (conj part item)
                                    score (+ score (scoref item))]
                                (if (>= score threshold)
                                  [(conj res part) [] 0]
                                  [res part score])))
                            [[] [] 0]
                            coll)]
     (if (empty? part)
       res
       (conj res part)))))


;; Don't like the "unpartionable data".  If you want sum to be less than 10 and you get 11,
;; you can't partition!  Just give up?  Skip that item and go on?  Use it anyway as a
;; singleton?  (No, violates invariant.)


;; SEM: name is potentially confusing.  Could be partition-accumulation?  but still issues.
(defn partition-with
  "Accumulates sequential items of <coll> into partitions.  First, an internal value is
  calculated by applying <accf> to previous accumulation and the incoming item.  If the
  resulting accumulation satifies <pred>, the item is added to the current partion.
  Otherwise, it goes into a new partition.  The internal accumulation starts as <init> for
  each partition.  Returns a stateful transducer when no collection is provided."

  {:added "1.X"
   :static true}
  ([pred accf init]
   (fn [rf]
     (let [a (java.util.ArrayList.)
           vacc (volatile! init)]
       (fn
         ([] (rf))
         ([result]
          (let [result (if (.isEmpty a)
                         result
                         (let [v (vec (.toArray a))]
                           ;;clear first!
                           (.clear a)
                           (unreduced (rf result v))))]
            (vreset! vacc nil)  ;; might help gc???
            (rf result)))
         ([result input]
          (let [acc (accf @vacc input)]
            (if (pred acc)
              ;; continue current partition
              (do
                (.add a input)
                (vreset! vacc acc)
                result)
              (if (.isEmpty a)
                ;; unpartitionable data
                (ensure-reduced result)
                ;; push old, start new partition
                (let [v (vec (.toArray a))]
                  (.clear a)
                  (let [ret (rf result v)
                        acc (accf init input)]
                    (if (pred acc)
                      (do
                        (.add a input)
                        (vreset! vacc acc)
                        ret)
                      ;; unpartitionable data
                      (ensure-reduced result))))))))))))


  ([pred accf init coll]
   (let [[res part] (reduce (fn [[res part acc] item]
                              (let [part (conj part item)
                                    acc (accf acc item)]
                                (if (pred acc)
                                  [(conj res part) [] init]
                                  [res part acc])))
                            [[] [] init]
                            coll)]
     (if (empty? part)
       res
       (conj res part)))))


;; SEM idea rotate (like partition-all but groups rotate???)

(defn rotate
  "Returns a lazy seq of the elements of the finite sequence `coll` with
  the first `n` elements rotated to the end of the result.  If n is
  negative, the last n elements of coll are rotated to the front of
  the result.  Default n is 1 if not given."
  ([coll] (rotate 1 coll))
  ([n coll]
       (let [cnt (count coll)
             n (if (<= cnt 1) 0 (mod n cnt))]
         (concat (drop n coll) (take n coll)))))

;; ray@1729.org.uk on clojure mailing list
(defn rotations [xs]
  "Returns a seq of all possible rotations of the finite seq `xs`"
  (take (count xs) (partition (count xs) 1 (cycle xs))))




;; experiment, but not exactly right
(defn take-while-then-map
  ([pred f]
   (fn [rf]
     (let [flagv (volatile! true)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [flag (and @flagv (pred input))]
            (if flag
              (rf result input)
              (rf result (f input)))))))))
  ([pred f coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (if (pred (first s))
          (cons (first s) (take-while-then-map pred f (rest s)))
          (map f s))))))


(defn take-then-map
  ([n f]
   (fn [rf]
     (let [nv (volatile! n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if-let [n @nv]
            (let [nn (vswap! nv dec)
                  result (if (pos? n)
                           (rf result input)
                           (rf result (f input)))]
              (when (zero? nn)
                (vreset! nv nil))
              result)
            (rf result (f input))))))))
  ([n f coll]
   (concat (take n coll) (map f (drop n coll)))))


(defn take-then
  ([n-or-f g]
   (if (fn? n-or-f)
     (take-while-then-map n-or-f g)
     (take-then-map n-or-f g)))
  ([n-or-f g coll]
   (if (fn? n-or-f)
     (take-while-then-map n-or-f g coll)
     (take-then-map n-or-f g coll))))
  
;; ([f]
;;     (fn [rf]
;;       (fn
;;         ([] (rf))
;;         ([result] (rf result))
;;         ([result input]
;;            (rf result (f input)))
;;         ([result input & inputs]
;;            (rf result (apply f input inputs))))))


;; user=> (sequence (take-then 4 (comp dec sq)) (range 20))
;; (0 1 2 3 15 24 35 48 63 80 99 120 143 168 195 224 255 288 323 360)

;; user=> (sequence (chain (take 4) (map (comp dec sq))) (range 20))

;; user=> (sequence (chain (take 4) (comp (map sq) (map dec))) (range 20))


;; See my-transmuters.txt
;; IDEA: (collect N xform ... rest-xform)



;; "Variadic partial fn".  For use in tranducers...
;;   (map (vfn max))
;; looks a bit nicer than any of these:
;;   (map #(apply max %))
;;   (map (partial apply max))
;;   (map (fn [args] (apply max args)))

;; Thought about other names: "partial*", "papp", "avfn", "apfn", "partial-apply"
;; But apply and derivatives are too active, it's not applying now.
;; partial* is maybe most logical, but a little long.
;; Liked vfn for being short and mnemonic, 'v' for "variadic".  Admittedly, it doesn't
;; create the variadic, it calls it.  So it's not a perfect name.

(defn vfn
  "Similar to `partial`, but for use a variadic function.  Takes variadic `f` and any number
  of fixed arguments.  Returns a function that takes a collection as its single argument and
  applies `f` to the concatenation of the fixed arguments and the collection argument."
  ([f] (fn [args] (apply f args)))
  ([f a] (fn [args] (apply f a args)))
  ([f a b] (fn [args] (apply f a b args)))
  ([f a b c] (fn [args] (apply f a b c args)))
  ([f a b c & more] (fn [args] (apply f a b c (concat more args)))))

