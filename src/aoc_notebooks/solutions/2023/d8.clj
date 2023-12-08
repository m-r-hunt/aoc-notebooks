;; # 2023 Day 8

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d8
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]
            [clojure.string :as string]))

;; [Problem](https://adventofcode.com/2023/day/8)

;; ## Part 1

(def input (input/line-groups 2023 8))

(def dirs (mapv (comp keyword string/lower-case str) (first input)))

(def lines (binding [input/*token-re* #"[ \t,\/\\':;\"#~{}()=]+"] (input/tokens (second input))))
(def network (into {} (mapv (fn [[src l r]] [src {:src src :l l :r r}]) lines)))

#_(loop [current :aaa steps 0]
  (println current)
  (if (= current :zzz)
    steps
    (let [instr (nth dirs (mod steps (count dirs)))]
      (recur (get-in network [current instr]) (inc steps)))))

;; ## Part 2

(def starts (filter #(string/ends-with? (str %) "a") (keys network)))

(defn solve
  [start]
  (loop [current start steps 0]
    (if (string/ends-with? (str current) "z")
      steps
      (let [instr (nth dirs (mod steps (count dirs)))]
        (recur (get-in network [current instr]) (inc steps))))))

(def times (map solve starts))

(apply utils/lcm times)
