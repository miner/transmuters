(ns miner.peak)


;; Original from Slack discussion

;; https://gist.github.com/madstap/a7d158ef0c3e7b5bbf5cd55c5de4c913#file-consecutive_transducer_thingy-clj

(defn merge-consecutive-by-with-xf ; this name is less than ideal...
  "Returns a transducer that reduces merge-fn over consecutive values for which
  (consecutive-by value) is the same."
  [consecutive-by merge-fn]
  (fn [rf]
    (let [prev (atom ::none)]
      (fn
        ([] (rf))
        ([res]
         (let [p @prev]
           (if (= ::none p)
             (rf res)
             (rf res p))))
        ([res x]
         (let [p @prev]
           (cond (= ::none p)
                 (do (reset! prev x)
                     res)

                 (= (consecutive-by p) (consecutive-by x))
                 (do (swap! prev merge-fn x)
                     res)

                 :else
                 (do (reset! prev x)
                     (rf res p)))))))))

;; SEM Notes:  as others pointed out, use volatile! instead of atom for transducers --
;; slightly faster, and you don't have to worry about thread isolation (because the caller
;; guarantees that only one thread will use the transducer.)
;; Bug: the [res] arity should call (rf (rf res p)) to give the upstream a chance to
;; complete.

;; I think the inputs should be maps (or some sort of associative data) in which case nil
;; could be used as the ::none sentinel value.  If inputs could be nil, then false or even
;; true could be used instead with the corresponding test (false? or true?).  The nil case
;; seems to be faster than the ::none.  Actually, identical? would have been better here,
;; than = ::none.

(defn SEM-merge-key-by
  [key-fn merge-fn]
  (fn [rf]
    (let [prev (volatile! nil)]
      (fn
        ([] (rf))
        ([res]
         (if-let [p @prev]
           (rf (rf res p))
           (rf res)))
        ([res x]
         (let [p @prev]
           (cond (nil? p)
                 (do (vreset! prev x)
                     res)

                 (= (key-fn p) (key-fn x))
                 (do (vswap! prev merge-fn x)
                     res)

                 :else
                 (do (vreset! prev x)
                     (rf res p)))))))))

(defn merge-key-by
  [key-fn merge-fn]
  (fn [rf]
    (let [prev (volatile! ::none)]
      (fn
        ([] (rf))
        ([res]
         (let [p @prev]
           (if (identical? ::none p)
             (rf res)
             (rf (rf res p)))))
        ([res x]
         (let [p @prev]
           (cond (identical? ::none p)
                 (do (vreset! prev x)
                     res)

                 (= (key-fn p) (key-fn x))
                 (do (vswap! prev merge-fn x)
                     res)

                 :else
                 (do (vreset! prev x)
                     (rf res p)))))))))



(defn merge-peaks
  "Merges two peaks, the result will retain the values of p1 but:
   - :end-time will be become the value of p2
   - :peak will be the concat' value of p1 and p2"
  [p1 p2]
  (-> p1
      (update :peak
              (fn [p1-peak new-peak]
                (vec (concat p1-peak new-peak)))
              (:peak p2))
      (update :end-time
              (fn [_ new-end-time] new-end-time)
              (:end-time p2))))

;; SEM notes:
;; (vec (concat x y)) -- better as (into x y) if you know x is vector.
;; second update call would be simpler as assoc, you're not using the old value
;; to "update".

(defn my-merge-peaks [p1 p2]
  (assoc p1
         :end-time (:end-time p2)
         :peak (into (:peak p1) (:peak p2))))
  
  




(comment

  (def test-peaks
    [{:type :a
      :peak [1 2]
      :end-time 1}
     {:type :a
      :peak [3 4]
      :end-time 2}
     {:type :a
      :peak [5 6]
      :end-time 3}
     {:type :b
      :peak []
      :end-time 8}])

  ;; Test
  (= [{:type :a, :peak [1 2 3 4 5 6], :end-time 3}
      {:type :b, :peak [], :end-time 8}]
     (into [] (merge-consecutive-by-with-xf :type merge-peaks) test-peaks))

  )



;;; SEM Comments
;; consecutive-by function is essentially a key-fn as used in min-key, etc.
;; my name might be: xmerge-by




;; ----------------------------------------------------------------------


;; Slack
;; noisesmith [8:56 PM] 
;; list subtraction as a transducer:
(defn list-
  ([targets]
   (let [victims (volatile! targets)]
     (fn ([rf]
          (fn
            ([] (rf))
            ([result]
             (rf result))
            ([result input]
             (if (and (seq @victims)
                      (= input (first @victims)))
               (do (vswap! victims rest)
                   result)
               (rf result input)))))))))

(comment
  (into [] (list- [1 2]) [1 1 2 3 4])
  ;; => [1 3 4]

  )



;; Do you really need to check (seq @victimes)?  Only if nil is allowed as an element.
;; prefer to deref only once.  Single arity fns don't need so many parens.
;; My clever name is drop-pop: you drop the first target, and then pop the target list.
;; Trying to be clever about empty target list.

;; Note: can't peek/pop targets because they can be any seq
;; (e.g. PersistentVector$ChunkedSeq), not necessarily a real list.

(defn drop-pop
  "If the input equals the first target, it is dropped and the target list is popped.
  Otherwise, the input is passed on without changing the target list."
  ([targets]
  (if-let [targets (seq targets)]
    (let [victims (volatile! targets)]
      (fn [rf]
        (fn
          ([] (rf))
          ([result]
           (rf result))
          ([result input]
           (if (let [vs @victims] (and (seq vs) (= input (first vs))))
             (do (vswap! victims rest)
                 result)
             (rf result input))))))
    ;; or use xpass
    (fn [rf]
      (fn
        ([] (rf))
        ([r] (rf r))
        ([r x] (rf r x))))))
  ([targets coll] (sequence (drop-pop targets) coll)))

;; conventional version
(defn dpop
  ([targets coll] (dpop targets coll []))
  ([targets coll result]
   (if (seq coll)
     (if-let [targets (seq targets)]
       (if (= (first targets) (first coll))
         (recur (rest targets) (rest coll) result)
         (recur targets (rest coll) (conj result (first coll))))
       (into result coll))
     result)))
