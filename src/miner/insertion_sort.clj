;; NOT A GOOD IDEA -- use built-in sort instead of this
(ns miner.insertion-sort
  (:use miner.transmuters))

;; ----------------------------------------------------------------------

;; insertion sort idea base on
;; http://www.johndcook.com/blog/2016/06/09/insertion-sort-as-a-fold/

;; slow and overly lazy
(defn insertx1 [xs x]
  (if-let [xs (seq xs)]
    (concat (filter #(< % x) xs) (list x) (filter #(>= % x) xs))
    (list x)))

;; issue: concat is lazy so lots of pending work
;; issue:  xs is already sorted so filter is doing too much, take-while would be better

;; "best" but seems not so great
(defn insertx2 [xs x]
  (if-let [xs (seq xs)]
    (into (filterv #(< % x) xs) (cons x (filter #(>= % x) xs)))
    (list x)))

;; still lots of vector conversions above

;; actually not better than insertx2
(defn insertx3 [xv x]
  (if (seq xv)
    (into (conj (filterv #(< % x) xv) x) (filter #(>= % x) xv))
    (vector x)))

;; a bit slower
(defn insertx4 [xs x]
  (if (seq xs)
    (let [[head tail] (split-with #(< % x) xs)]
      (-> [] (into head) (conj x) (into tail)))
    (list x)))

;; like 4
(defn insertx5 [xs x]
  (if (seq xs)
    (let [ltx #(< % x)]
      (-> [] (into (take-while ltx xs)) (conj x) (into (drop-while ltx xs))))
    (list x)))

(defn insertx6 [xs x]
  (if (seq xs)
    (let [ltx #(< % x)]
      (transduce (chain (take-while ltx) pushback (prepend [x])) conj [] xs))
    (vector x)))

;; almost better
(defn insertx7 [xv x]
  (if-let [xs (seq xv)]
    (let [head (into [] (take-while #(< % x) xs))]
      (into (conj head x) (subvec xv (count head))))
    (vector x)))

;; no better
(defn insertx8 [xv x]
  (if-let [xs (seq xv)]
    (let [head (reduce conj! (transient []) (take-while #(< % x) xs))]
      (persistent! (reduce conj! (conj! head x) (drop-while #(< % x) xs))))
    (vector x)))


(defn index-of [pred xs]
  (first (keep-indexed (fn [i v] (when (pred v) i)) xs)))

;; too slow 
(defn insertx9 [xv x]
  (if-let [xs (seq xv)]
    (let [offset (index-of #(>= % x) xs)
          head (subvec xv 0 offset)]
      (into (conj head x) (subvec xv offset)))
    (vector x)))


(defn insertion-sort [xs]
  (reduce insertx [] xs))


(defn resort [xs x]
  (sort (conj xs x)))


(comment
  (def zzz (interleave (range 500) (range 1000 500 -1)))

  (time (last (reduce insertx7 [9999] zzz)))

  (time (last (transduce conj (completing insertx7) [9999] zzz)))

  ;; Note: much faster to just use sort
  (time (last (sort (conj zzz 9999))))
  
  )
