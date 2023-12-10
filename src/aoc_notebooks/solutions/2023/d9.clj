;; # 2023 Day 9

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d9
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/9)

;; ## Part 1

;; Parsing is really easy today, just numbers

(def input (input/numbers 2023 9))

;; We're going to bang out a quick recursive function.
;; It takes a list of numbers, checks if they are all zero. If so, return zero.
;; Otherwise create a collection of all the differences between elements, and recurse, adding on the recursive result to the last element.
;; This succesfully calculates the next number in the sequence at the top level.

(defn recursive-diffs
  [coll]
  (if (every? zero? coll)
    0
    (+ (last coll) 
       (recursive-diffs (map - (rest coll) coll)))))

(recursive-diffs (first input))
(recursive-diffs (second input))
(recursive-diffs (nth input 2))

(reduce + (map recursive-diffs input))

;; ## Part 2

;; Part 2 is very easy today, just run the same function on the list of numbers reversed.

(recursive-diffs (reverse (nth input 2)))

(reduce + (map (comp recursive-diffs reverse) input))
