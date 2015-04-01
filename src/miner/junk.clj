;; fragments that didn't work, but maybe will remind me of something
;; at least, what not to do



;; mostly works but clunky, keep for testing
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
  (let [n (count xs)]
    (take n (partition n 1 (cycle xs)))))




;; DOESN'T HELP -- issue is (take 0) allows forces input to be swallowed.  Want a way to put
;; back unwanted input???

(defn my-take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n.  Returns a stateful transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([n]
   (if (<= n 0)
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input] (ensure-reduced result))))
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [n @nv
                    nn (vswap! nv dec)
                    result (if (pos? n)
                             (rf result input)
                             result)]
                (if (not (pos? nn))
                  (ensure-reduced result)
                  result))))))))
  ([n coll]
     (lazy-seq
      (when (pos? n) 
        (when-let [s (seq coll)]
          (cons (first s) (take (dec n) (rest s))))))))





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

