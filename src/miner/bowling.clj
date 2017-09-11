(ns miner.bowling
  (:require [miner.transmuters :as tm]))

;; http://codingdojo.org/cgi-bin/index.pl?KataBowling
;; https://www.javacodegeeks.com/2016/05/bowling-kata-clojure-f-scala.html

;; game is a string, encoding balls rolled
;; X for strike
;; / for spare
;; - for a miss or gutter ball
;; 1-9 for that many pins
;; As an extension, spaces are allowed but ignored in the game string purely for readability.

;; If last frame is a strike or spare, the extra balls only accrue to that frame.

;; A "ball" re-encodes the character as 0 to 10 or :spare.  This is for readability and
;; more convenient scoring.


(defn ch->ball [ch]
  (case ch
    \X 10
    \/ :spare
    \- 0
    (- (long ch) (long \0))))

(defn ball-seq [game]
  (map ch->ball (remove #{\space} game)))


;; Note, a :spare is scored only on the second ball, so we ignore the ball A if the
;; next B is a :spare.  By the way, it's illegal to have two :spare in a row or a :spare
;; immediately after a strike (10) but we don't check.

;; (A B C) are three sequential balls, triplet-score returns the score assigned for A.
(defn triplet-score [[a b c]]
  (case a
    :spare (+ 10 b)
    10 (case c
         :spare 20
         (+ 10 b c))
    (case b
      :spare 0
      a)))


(defn ten-frames-count [balls]
  (loop [balls balls cnt 0 halves 0]
    (if (or (= halves 20) (empty? balls))
      cnt
      (recur (rest balls) (inc cnt) (if (= (first balls) 10) (+ halves 2) (inc halves))))))

;; sadly, attempt with reduce not better than loop version


;; Each ball might need up to two additional balls to calculate a score so we partition into
;; triplets.  We ignore the extraneous triplets that come from extra balls used only to
;; score a final frame strike or spare.

(defn score [game]
  (let [balls (ball-seq game)
        tenframes (ten-frames-count balls)]
    (reduce + (map triplet-score (take tenframes (partition-all 3 1 balls))))))



(defn smoke-test []
  (and (= (score "35 6/ 7/ X 45 XXXXXXX") 223)
       (= (score "11 11 11 11 11 11 11 11 X 11") 30)
       (= (score "XXXXXXXXXXXX") 300)
       (= (score "9-9-9-9-9-9-9-9-9-9-") 90)
       (= (score "5/5/5/5/5/5/5/5/5/5/5") 150)))




;; as a bonus, we should write a validator to confirm that the original game string is legal
;; two balls in a frame should be less than 10 (otherwise spare or strike)
;; correct ending frame and extra balls
;; exactly 10 frames

(defn strike? [[a b]]
  (and (nil? b) (= a 10)))

(defn spare? [[a b]]
  (and (not= a :spare) (<= 0 a 9) (= b :spare)))

(defn frames
  ([balls] (frames balls [] []))
  ([balls frame acc]
   (cond (empty? balls) (if (seq frame) (conj acc frame) acc)
       (= (first balls) 10) (recur (rest balls) [] (conj acc (conj frame 10)))
       (seq frame) (recur (rest balls) [] (conj acc (conj frame (first balls))))
       :else (recur (rest balls) (conj frame (first balls)) acc))))


(defn valid-frame? [frame]
  (or (strike? frame)
      (spare? frame)
      (let [[a b] frame]
        (and (not= a :spare)
             (not= b :spare)
             (<= 0 a 9) 
             (<= 0 b 9)
             (< (+ a b) 10)))))

;; We need to be careful about the extra balls after the tenth frame and how they get
;; (accidentally) grouped into frames, particularly strikes which always appear as isolated
;; frames.

(defn valid-balls? [balls]
  (let [fs (frames balls)
        cnt (count fs)]
    (and (<= 10 cnt 12)
         (every? valid-frame? (take 10 fs))
         (let [[f3 f2 f1] (take-last 3 fs)]
           (or (and (= cnt 12)
                    (strike? f1)
                    (strike? f2)
                    (strike? f3))
               (and (= cnt 11)
                    (strike? f2)
                    ;; last two balls look like a frame, but not really
                    (= (count f1) 2)
                    (every? integer? f1) ;; not :spare
                    (<= (reduce + f1) 10))
               (and (= cnt 11)
                    (spare? f2)
                    (= (count f1) 1)
                    (integer? (first f1)) ;; not :spare
                    (<= (first f1) 10))
               (and (= (count fs) 10)
                    (not (strike? f1))
                    (not (spare? f1))))))))

(defn valid-game? [game]
  (valid-balls? (ball-seq game)))


(defn validation-test []
  (and (every? valid-game?
               ["35 6/ 7/ X 45 XXXXX XX" "XXXXXXXXXXXX"  "9-9-9-9-9-9-9-9-9-9-"
                "5/5/5/5/5/5/5/5/5/5/5"])
       (not-any? valid-game?
                 ["35 6/ 7/ X 45 XXXXX X" "35 6/ 7/ X 45 XXXXX X/" "35 6/ 7/ X 45 XXXXX 1X"
                  "XXXXXXXXXXX" "XXXXXXXXXXX/"
                  "9-9-9-9-9-9-9-9-9-9-3" "9-9-9-9-9-9-9-9-9-9X" "9-9-9-9-9-9-9-9-9-9X"
                  "5/5/5/5/5/5/5/5/5/5/" "5/5/5/5/5/5/5/5/5/5X" "5X5/5/5/5/5/5/5/5/5/5"])))




;; needs proper framing to calculate when "done" with tenth frame
;; tricky partioning and cat

(defn tscore1 [game]
  (transduce (comp (remove #{\space}) (map ch->ball) 
                 (tm/slide-all 3)
                 (tm/partition-threshold #(if (= (first %) 10) 2 1) 2)
                 (take 10) cat
                 (map triplet-score))
           + 0
           game))


(defn tscore [game]
  (transduce (comp (remove #{\space})
                   (map ch->ball) 
                   (tm/slide-all 3)
                   (tm/partition-threshold #(if (= (first %) 10) 2 1) 20)
                   (take 1)
                   cat
                   (map triplet-score))
             + 0
             game))

;; transducer version seems slower, probably too much messing with framing

(defn tsmoke-test []
  (and (= (tscore "35 6/ 7/ X 45 XXXXXXX") 223)
       (= (tscore "11 11 11 11 11 11 11 11 X 11") 30)
       (= (tscore "XXXXXXXXXXXX") 300)
       (= (tscore "9-9-9-9-9-9-9-9-9-9-") 90)
       (= (tscore "5/5/5/5/5/5/5/5/5/5/5") 150)))

