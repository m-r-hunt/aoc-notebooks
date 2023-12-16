;; # 2023 Day 14

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d14
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/14)

;; ## Part 1

(def input (input/grid 2023 14))

(input/draw-grid input)

(defn walk-up-to-blocker
  [grid [x y]]
  (first (filter #(and (not= (get-in grid [:data %]) \O) (not= (get-in grid [:data %]) \.)) 
                 (map vector (repeat x) (reverse (range -1 y)) ))))

(walk-up-to-blocker input [0 4])
(walk-up-to-blocker input [4 3])
(walk-up-to-blocker input [5 4])

(defn walk-down-to-space
  [grid [bx by] [ox oy]]
  (or (first (filter #(= (get-in grid [:data %]) \.) 
                     (map vector (repeat bx) (range by oy))))
      [ox oy]))

(walk-down-to-space input [0 -1] [0 4])
(walk-down-to-space input [4 1] [4 3])
(walk-down-to-space input [5 2] [5 4])

(defn move-boulder-up
  [grid c]
  (if (= (get-in grid [:data c]) \O)
    (let [blocker (walk-up-to-blocker grid c)
          target (walk-down-to-space grid blocker c)]
      (-> grid
          (assoc-in [:data target] \O)
          (assoc-in [:data c] (get-in grid [:data target]))))
    grid))

(def rolled (reduce move-boulder-up input (keys (:data input))))

(input/draw-grid rolled)

(reduce + (map (fn [[[x y] c]] (if (= c \O) (- (:height rolled) y) 0)) (:data rolled)))

;; ## Part 2
