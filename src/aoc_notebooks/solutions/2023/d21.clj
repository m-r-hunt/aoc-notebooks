;; # 2023 Day 21

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d21
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/21)

;; ## Part 1

(def input (input/grid 2023 21))

(input/draw-grid input)

(def start (first (first (filter (fn [[c x]] (= x \S)) (:data input)))))

(defn unseen-neighbours
  [[cx cy] seen]
  (filter #(= \. (get-in input [:data %])) (filter (comp not seen) [[(dec cx) cy]
                                                             [(inc cx) cy]
                                                             [cx (dec cy)]
                                                             [cx (inc cy)]])))

(def steps 64)

(def results (loop [frontier [start] results #{start} seen #{start} iteration 0]
               (if (or (>= iteration steps)
                       (empty? frontier))
                 results
                 (let [next (into #{} (apply concat (map unseen-neighbours frontier (repeat seen))))
                       results (if (= (mod iteration 2) 1) (apply conj results next) results)]
                   (recur next results (apply conj seen next) (inc iteration))))))

(count results)

(input/draw-grid input :filter (fn [c d] (if (results c) \O d)))

;; ## Part 2
