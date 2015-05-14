(ns miner.transmuters)




(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))


;; app - helper for handling variadic functions in transducers...
;;   (map (app max))
;;
;; looks a bit nicer than any of these:
;;   (map #(apply max %))
;;   (map (partial apply max))
;;   (map (fn [args] (apply max args)))

;; "app" is shortened "apply", somewhat mnemonic.  Looks good in a transducer sequence.

(defn app
  "Similar to `partial`, but for use a variadic function.  Takes variadic `f` and any number
  of fixed arguments.  Returns a function that takes a collection as its single argument and
  applies `f` to the concatenation of the fixed arguments and the collection argument."
  ([f] (fn [args] (apply f args)))
  ([f a] (fn [args] (apply f a args)))
  ([f a b] (fn [args] (apply f a b args)))
  ([f a b c] (fn [args] (apply f a b c args)))
  ([f a b c & more] (fn [args] (apply f a b c (concat more args)))))


;; numbering scheme follows anonymous function notation
(defn farg
  "Returns a function that simply returns its Nth arg.  The first arg is position 1, which
  is the default.  If there is no corresponding arg, the default-value is returned, which
  defaults to nil."
  ([] (farg 1 nil))
  ([n] (farg n nil))
  ([n default-value]
   (fn
     ([] default-value)
     ([a] (case n 1 a default-value))
     ([a b] (case n 1 a 2 b default-value))
     ([a b c] (case n 1 a 2 b 3 c default-value))
     ([a b c & args] (case n 1 a 2 b 3 c (nth args (- n 4) default-value))))))



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


;; Transducers support early termination using (reduced ...). Some of those early
;; terminators, such as take-while, will burn an extra input because it needs to know
;; when to stop.  Other examples: xempty and (take 0) -- but not (take N) where N is positive.
;; In these cases, we typically want the next xform in the chain to see that burned input.
;; To get the desired behavior, the chain call must have `pushback` (no parens) as the next item.
;; Note that `pushback` is not a real transducer, but the `chain` fn handles it specially to
;; reuse the input with the next xform in the chain.  Obviously, it doesn't make sense to
;; use `pushback` as the first or last item in a transducer chain, or to have multiple
;; pushbacks in a row.

(defn pushback
  "A pseudo-transducer which indicates to `chain` that the same input should be re-used by the
  next xform in the chain.  It should be used in a chain following a transducer such as
  `take-while` which normally burns an extra input when deciding to terminate.  For example,
  (chain (take-while even?) pushback (take 5)) would avoid skipping the odd number that
  terminated the take-while."
  [_]
  ::pushback)


;; SEM -- consider step with multiple results, then reduced.  Does that work?

;; Issue -- not sure if (chain) should be like identity (just pass through) or maybe
;; terminate immediately (empty list).

(defn chain
  "Chains together any number of transducers such that when one transducer terminates, the
  next transducer can pick up where it left off.  If a transducer never terminates, the
  following ones will never see any input.  If a terminating transducer such as `take-while`
  burns an extra input to decide when to terminate, that input will appear to be skipped by
  the chain. You can use the pseudo-transducer `pushback` as the next transducer in the
  chain to give that following transducer access to that item as its first input."
  {:static true}

  ([] identity)

  ([xf] xf)

  ([xf & xfs]
   (fn [rf]
     (let [vxfs (volatile! (map #(% rf) (conj xfs xf)))]
       (fn
         ([] (if-let [xform (first @vxfs)] (xform) (rf)))
         ([result] (if-let [xform (first @vxfs)] (xform result) (rf result)))
         ([result input]
          (if-let [xform (first @vxfs)]
            ;; treat initial or duplicate pushback as a no-op
            (if (= xform ::pushback)
              (do (vswap! vxfs next)
                  (recur result input))
              (let [res (xform result input)]
                ;; reduced means that xform is done, so we deref and complete
                ;; xform = ::pushback is a special case where we want the next xform to use
                ;; the same input (typically after a take-while).
                (if (reduced? res)
                  ;; completion with original xform
                  (if (= (first (vswap! vxfs next)) ::pushback)
                    (do (vswap! vxfs next)
                        (recur (xform @res) input))
                    (xform @res))
                  res)))
            (ensure-reduced result))))))))

;; I think (take-nth 0) should always be the empty list.
;; This is a change from the way it works in Clojure 1.6.
;; Clojure 1.7.0-alpha5 throws on (take-nth 0), mine gives a transducer that will return ().
;; See also http://dev.clojure.org/jira/browse/CLJ-1665
(defn my-take-nth
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided.  N=0 returns empty list."
  {:static true}
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


(defn- lazy-drop-nth [n coll]
  ;; assumes (pos? n)
  ;; Notice drops first (aiming to be complement of take-nth)
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (take (dec n) (rest s)) (lazy-drop-nth n (drop n s))))))

(defn drop-nth
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
     (lazy-seq coll)
     (lazy-drop-nth n coll))))


;; collection version by Frank on mailing list
;; SEM added transducer version, adapted from partition-by
(defn partition-when
   "Applies f to each value in coll, starting a new partition each time f returns a
   true value.  Returns a lazy seq of partitions.  Returns a stateful
   transducer when no collection is provided."
  {:static true}
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



(defn partition-threshold
  "Accumulates sequential items of <coll> into partitions according to <scoref> function and
  the <threshold> integer.  As each item goes into the current partition, an integral score
  is calculated by applying the <scoref> function to the incoming item and adding the result
  to the previous score.  If the resulting score is greater than or equal to the
  <threshold>, the current partition is complete and the next item will go into a new
  partition.  The internal score starts as 0 for each partition.  Returns a stateful
  transducer when no collection is provided."
  {:static true}
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



(defn remove-once
  ([pred] (chain (take-while (complement pred)) identity))
  ([pred coll]
   (let [[head tail] (split-with (complement pred) coll)]
     (concat head (rest tail)))))


;; generalized take-while
(defn take-while-accumulating [accf init pred2]
  "Similar to take-while, but tests with an internal 'accumulation' state and the incoming
   input.  accf is like a reducing function: it takes two args, state and input, and returns
   new state of the “accumulation”.  init is the initial state of the accumulation.  pred2
   is a predicate taking two args, the accumulation state and the new input.  The process
   stops when pred2 returns false."
   (fn [rf]
     (let [vstate (volatile! init)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (pred2 @vstate input)
            (do (vswap! vstate accf input)
                (rf result input))
            (reduced result)))))))

;; "until" just complements the predicate
(defn take-until-accumulating [accf init pred2]
  (take-while-accumulating accf init (complement pred2)))

;; think about drop-while-accumulating

#_ (defn rolling-window [f init]
     ;; NOT IMPLMENTED YET
  "f takes a single argument, which should be a sequence.  init is the initial sequence.  A
  queue of the same size as init is maintained by pushing from the input sequence and
  popping the old (first) value off the queue."
  )
