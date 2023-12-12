;; # 2023 Day 12

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d12
  (:require [aoc-notebooks.input :as input]
            [clojure.string :as str]))

;; [Problem](https://adventofcode.com/2023/day/12)

;; ## Part 1

(def input (input/split-lines 2023 12))

(defn parse
  [line]
  (let [[row nums] (str/split line #" ")
        nums (mapv #(Long/parseLong %) (str/split nums #","))]
    {:row row :nums nums}))

(def rows (mapv parse input))

(map (fn [{:keys [:row]}] (bit-shift-left 1 (count (filter #{\?} row)))) rows)
(reduce + (map (fn [{:keys [:row]}] (bit-shift-left 1 (count (filter #{\?} row)))) rows))

(defn check
  [row counts]
  (let [actual-counts (vec (filter #(not= 0 %) (mapv count (str/split row #"\."))))]
    (= counts actual-counts)))

(check "#.#.###" [1 1 3])
(check ".###.##....#" [3 2 1])

(defn num-valid
  [row counts]
  (if (re-find #"\?" row)
    (+ (num-valid (str/replace-first row \? \.) counts)
       (num-valid (str/replace-first row \? \#) counts))
    (if (check row counts) 1 0)))

(num-valid "#.#.###" [1 1 3])
(num-valid "???.###" [1 1 3])
(num-valid "?###????????" [3 2 1])

(def res (map #(num-valid (:row %) (:nums %)) rows))
(reduce + res)

;; ## Part 2
