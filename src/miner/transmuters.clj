(ns miner.transmuters)

;; Just playing around with transducers, new in Clojure 1.7.
;; http://clojure.org/transducers

;; By the way, I don't like the signature of the new `eduction` ([xform* coll]).  It's strange
;; to take multiple xforms and then one trailing coll.  Normally, multiple whatevers would
;; go last, after some fixed number of args.  The counter-argument (per RH) is that these sorts of
;; functions take the collection as the last arg.  I know I can't win on this one, but I can
;; write my own little function.

(defn xduction [coll & xfs]
  "Variant of eduction that takes the coll as first arg, the rest are transducers"
  (->Eduction (apply comp xfs) coll))

;; simple source transfomation:
;; (->> (range 50) (take-nth 3) (filter even?))
;; (xduction (range 50) (take-nth 3) (filter even?))


(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;; (assert (pos? limit))
;; (assert (instance? clojure.lang.PersistentQueue q))
(defn push
  ([q x] (conj q x))
  ([q limit x]
   (if (>= (count q) limit)
     (recur (pop q) limit x)
     (conj q x))))

;; app - helper for handling variadic functions in transducers...
;;   (map (app max))
;;
;; looks a bit nicer than any of these:
;;   (map #(apply max %))
;;   (map (partial apply max))
;;   (map (fn [coll] (apply max coll)))

;; "app" is shortened "apply", somewhat mnemonic.  Looks good in a transducer sequence.

(defn app
  "Similar to `partial`, but for use a variadic function.  Takes variadic `f` and any number
  of fixed arguments.  Returns a function that takes a collection as its single argument and
  applies `f` to the concatenation of the fixed arguments and the collection argument."
  ([f] (fn [coll] (apply f coll)))
  ([f a] (fn [coll] (apply f a coll)))
  ([f a b] (fn [coll] (apply f a b coll)))
  ([f a b c] (fn [coll] (apply f a b c coll)))
  ([f a b c & more] (fn [coll] (apply f a b c (concat more coll)))))

(defn appmap
  "variant of map-map, helper for transducing functions that map across an input (collection)"
  ([f] (fn [coll] (map f coll)))
  ([f a] (fn [coll] (map f a coll)))
  ([f a b] (fn [coll] (map f a b coll)))
  ([f a b c] (fn [coll] (map f a b c coll)))
  ([f a b c & more] (fn [coll] (apply map f a b c (concat more (list coll))))))


;; numbering scheme follows anonymous function notation
(defn farg
  "Returns a function that simply returns its Nth arg.  The first arg is position 1, which
  is the default.  If there is no corresponding arg, the default-value is returned, which
  defaults to nil."
  ([] (farg 1 nil))
  ([n] (farg n nil))
  ([^long n default-value]
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
  "Transducer variant of empty."
  ([] (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result)))))
  ([coll] (empty coll)))


(defn xlast
  "Stateful transducer variant of last."
  ([]
   (fn [rf]
     (let [xv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (let [x @xv]
                     (if (identical? x ::none)
                       (rf result)
                       (rf (rf result x)))))
         ([result input]
          (vreset! xv input)
          result)))))
  ([coll] (list (last coll))))

(defn xfirst
  "Transducer variant of first."
  ([] (fn [rf]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input] (ensure-reduced (rf result input))))))
  ([coll] (list (first coll))))



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

(defn latch
  "Stateful transducer that latches onto an input if it satisfies pred.  Any input received
  before that is ignored, producing no result.  Once a value is latched, it repeats as the
  result value for subsequent input until some input satisifies reset-pred, resetting the
  latch.  The default reset-pred is (constantly false).  The default pred is identity, which
  naturally latches onto the first input."
  ([] (latch identity))
  ([pred] (latch pred (constantly false)))
  ([pred reset-pred]
   (fn [rf]
     (let [xv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [x @xv]
            (if (identical? x ::none)
              (if (pred input)
                (do (vreset! xv input)
                    (rf result input))
                result)
              (if (reset-pred input)
                (do (vreset! xv ::none)
                    result)
                (rf result x))))))))))


;; sort of a filter with state
(defn switch
  "Stateful transducer that activates when an input satisfies pred, and deactivates when an
  input satisfies pred-reset.  When in the active state, input values are returned in the
  results.  When deactivated, no results are produced.  The default pred-reset is
  (constantly false), in effect a one-shot switch."
  ([pred] (switch pred (constantly false)))
  ([pred pred-reset]
  (fn [rf]
    (let [vactive (volatile! false)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (if @vactive
             (if (pred-reset input)
               (do (vreset! vactive false)
                   result)
               (rf result input))
             (if (pred input)
               (do (vreset! vactive true)
                   (rf result input))
               result))))))))

(defn counter
  ([] (counter identity))
  ([pred]
   (fn [rf]
     (let [nv (volatile! 0)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (pred input)
            (rf result (vswap! nv inc))
            (rf result false))))))))


;; SEM other similar ideas:
;; counter (returns number of times pred has been satisifed)
;; transitor pred decides to turn on or off or amplifiy signal -- isn't that just map?
;; What's a three-terminal analogue to a signal processing device?


;; almost matching behavior, but transducer returns finite (instead of infinite) list on
;; N=0.  Treats neg N like 0, which is more like collection version, although there's still
;; the finite vs. infinite issue.
(defn my-take-nth
  "Returns a lazy seq of the first item and every nth item thereafter in coll.
  Returns a stateful transducer when no collection is provided.  If N is not positive,
  returns a infinite sequence of first item in coll."  
  {:static true}
  ([n]
   (if-not (pos? n)
     (latch)
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
  ;; standard
  ([n coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (cons (first s) (my-take-nth n (drop n s)))))))

(defn repeat-nth [n]
  (comp (drop (dec n)) (latch)))
  

(defn- lazy-drop-nth [n coll]
  ;; assumes (pos? n)
  ;; Notice: drops first, and every nth after.  Goal is to be complement of take-nth.
  (lazy-seq
   (when-let [s (seq coll)]
     (concat (take (dec n) (rest s)) (lazy-drop-nth n (drop n s))))))

(defn drop-nth
  "Returns a lazy seq dropping the first and every nth item thereafter in coll.  Returns a stateful
  transducer when no collection is provided.  When N <= 0, returns the (rest coll)."
  {:static true}
  ([n]
   (if-not (pos? n)
     (drop 1)
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
   (if-not (pos? n)
     (lazy-seq (rest coll))
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


;; slide is like a step=1 partition (as opposed to partition-all)
;; if you want a different step size, compose with take-nth
;;   (comp (partitions 3) (take-nth 3)) -- similar to partion (but not -all)
;; internal queue is initialized to init (default [])

;; (partition-all 4 1 (range 5))
;;=> ((0 1 2 3) (1 2 3 4) (2 3 4) (3 4) (4))
;; (partition 4 1 (range 5))
;;=> ((0 1 2 3) (1 2 3 4))
;; (sequence (slide 4) (range 5))
;;=> ((0 1 2 3) (1 2 3 4))

;; was called partitions, now slide
;; (assert (pos? n))
(defn slide
  ([n] (slide n []))
  ([n init]
   (fn [rf]
     (let [qv (volatile! (queue init))]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [q (vswap! qv push n input)]
            (if (< (count q) n)
              result
              (rf result (seq q))))))))))


;; transducer version of reductions, slightly different regarding required init, and no
;; output for init
(defn accumulations [accf init]
  (fn [rf]
    (let [state (volatile! init)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
         (rf result (vswap! state accf input)))))))

(defn convolve [kernel]
  (comp (slide (count kernel)) (map (appmap * kernel)) (map (app +))))


;; IDEA: a transducer that takes its parameters from the input.  Kind of mixing control and
;; signal, but it might be useful.

;; ISSUE: some of my transducers are making little collections along the way just so they
;; can be processed by the next step.  Can we instead subtransduce?  And avoid making the
;; little collections along the way?
