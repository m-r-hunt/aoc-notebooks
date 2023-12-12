;; # 2023 Day 11

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d11
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/11)

;; ## Part 1

(def input (input/grid 2023 11))

(input/draw-grid input)

(def galaxies (mapv first (filter #(= (second %) \#) (:data input))))

(count galaxies)

(/ (* (count galaxies) (dec (count galaxies))) 2)

(def empty-rows (into #{} (filter #(every? (fn [c] (= \. (get-in input [:data c]))) (map vector (range 0 (:width input)) (repeat %))) (range 0 (:height input)))))
(def empty-cols (into #{} (filter #(every? (fn [c] (= \. (get-in input [:data c]))) (map vector (repeat %) (range 0 (:height input)))) (range 0 (:width input)))))

(defn cost
  [[x1 y1] [x2 y2]]
  (let [base (+ (abs (- x2 x1)) (abs (- y2 y1)))
        extra-rows (count (filter empty-rows (range (min y1 y2) (max y1 y2))))
        extra-cols (count (filter empty-cols (range (min x1 x2) (max x1 x2))))]
    (+ base extra-rows extra-cols)))

(cost [1 5] [4 9]) ; 9
(cost [4 9] [1 5]) ; 9

(defn to-every-galaxy
  [g]
  (reduce + (map cost (repeat g) galaxies)))

(to-every-galaxy [1 5])

(/ (reduce + (map to-every-galaxy galaxies)) 2)

;; ## Part 2

(def expansion 1000000)

(defn cost2
  [[x1 y1] [x2 y2]]
  (let [base (+ (abs (- x2 x1)) (abs (- y2 y1)))
        extra-rows (* (dec expansion) (count (filter empty-rows (range (min y1 y2) (max y1 y2)))))
        extra-cols (* (dec expansion) (count (filter empty-cols (range (min x1 x2) (max x1 x2)))))]
    (+ base extra-rows extra-cols)))

(defn to-every-galaxy2
  [g]
  (reduce + (map cost2 (repeat g) galaxies)))

(/ (reduce + (map to-every-galaxy2 galaxies)) 2)
