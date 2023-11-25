;; # 2022 Day 2

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2022.d2
  (:require [aoc-notebooks.input :as input]))

;; [Problem](https://adventofcode.com/2022/day/2)

;; ## Part 1

(def input (input/tokens 2022 2))

;; We can split the score calculation for a round into points we get for playing a shape and points for the result.
;; Points for shape played is quite simple.

(defn played-score
  [ours]
  (case ours
    :x 1
    :y 2
    :z 3))

(played-score :x) ; 1
(played-score :y) ; 2
(played-score :z) ; 3

;; Points for the result is trickier, we can convert the shapes into numbers mod 3, which makes calculating draws easy.
;; The code for wins is still hairy, there's probably a nice way to do it.

(defn result-score
  [theres ours]
  (let [theres-conv (case theres
                      :a 0
                      :b 1
                      :c 2)
        ours-conv (case ours
                    :x 0
                    :y 1
                    :z 2)]
    (cond
      (= theres-conv ours-conv) 3
      (or (and (= theres-conv 0) (= ours-conv 1))
          (and (= theres-conv 1) (= ours-conv 2))
          (and (= theres-conv 2) (= ours-conv 0))) 6
      :else 0)))

(result-score :a :x) ; 3
(result-score :a :y) ; 6
(result-score :a :z) ; 0

;; Full score then just adds the components.

(defn score
  [theres ours]
  (+ (played-score ours) (result-score theres ours)))

(score :a :x) ; 4
(score :a :y) ; 8
(score :a :z) ; 3

;; And to get the final answer, just sum up all the rounds

(apply + (map #(apply score %) input))

;; ## Part 2

;; Part 2 is kind of like part 1, but flipped. It's now easy to calculate results.

(defn result-score2
  [ours]
  (case ours
    :x 0
    :y 3
    :z 6))

;; But trickier to calculate what we played. Just use a nasty conditional as it's easy.

(defn played-score2
  [theres ours]
  (cond
    (or (and (= theres :a) (= ours :y))
        (and (= theres :b) (= ours :x))
        (and (= theres :c) (= ours :z))) 1
    (or (and (= theres :a) (= ours :z))
        (and (= theres :b) (= ours :y))
        (and (= theres :c) (= ours :x))) 2
    :else 3))

(played-score2 :a :y) ; 1
(played-score2 :b :x) ; 1

;; Finishing off and getting the final answer is the same really.

(defn score2
  [theres ours]
  (+ (played-score2 theres ours) (result-score2 ours)))

(apply + (map #(apply score2 %) input))
