;; # 2022 Day 2

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2022.d2
  (:require [aoc-notebooks.input :as input]))

;; [Problem](https://adventofcode.com/2022/day/2)

;; ## Part 1

(def input (input/tokens 2022 2))

(defn played-score
  [ours]
  (case ours
    :x 1
    :y 2
    :z 3))

(played-score :x)
(played-score :y)
(played-score :z)

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

(result-score :a :x)
(result-score :a :y)
(result-score :a :z)

(defn score
  [theres ours]
  (+ (played-score ours) (result-score theres ours)))

(score :a :x)
(score :a :y)
(score :a :z)

(apply + (map #(apply score %) input))

;; ## Part 2

(defn result-score2
  [ours]
  (case ours
    :x 0
    :y 3
    :z 6))

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

(played-score2 :a :y) ;; 1
(played-score2 :b :x) ;; 1

(defn score2
  [theres ours]
  (+ ( played-score2 theres ours) (result-score2 ours)))

(apply + (map #(apply score2 %) input))
