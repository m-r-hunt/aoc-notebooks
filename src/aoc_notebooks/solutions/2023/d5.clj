;; # 2023 Day 5

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d5
  (:require [aoc-notebooks.input :as input]))

;; [Problem](https://adventofcode.com/2023/day/5)

;; ## Part 1

(def input (mapv input/numbers-and-tokens (input/line-groups 2023 5)))

(def seeds (vec (rest (first (nth input 0)))))

(defn make-range
  [[dest src size]]
  {:src src :dest dest :size size})

(defn parse-map
  [[name] & ranges]
  {:name name :ranges (mapv make-range ranges)})

(apply parse-map (second input))

(def maps (mapv #(apply parse-map %) (rest input)))

(defn map-number
  [n ranges]
  (if-let [{:keys [dest src size]} (first (filter (fn [{:keys [dest src size]}] (<= src n (+ src size -1))) ranges))]
    (+ dest (- n src))
    n))

(map-number (first seeds) (:ranges (first maps)) )
(map :ranges maps)
(reduce map-number (first seeds) (map :ranges maps))

(def all-ranges (map :ranges maps))

(reduce min (map #(reduce map-number % all-ranges) seeds))

;; ## Part 2

(defn map-number-range
  [{:keys [start range-size] :as orig} ranges]
  (if-let [{:keys [dest src size]} (first (filter (fn [{:keys [dest src size]}] (<= src start (+ src size -1))) ranges))]
    (let [rest-size (- (+ src size) start)]
      (if (>= rest-size range-size)
        [{:start (+ dest (- start src)) :range-size range-size}]
        (conj (map-number-range {:start (+ start rest-size) :range-size (- range-size rest-size)} ranges) {:start (+ dest (- start src)) :range-size rest-size}))) 
    (if-let [new-start (first (filter #(not= % (map-number % ranges)) (range start (+ start range-size))))]
      (conj (map-number-range {:start new-start :range-size (- range-size (- new-start start))}
                              ranges)
            {:start start :range-size (- new-start start)})
      [orig])))

(def seed-ranges (mapv (fn [[start range-size]] {:start start :range-size range-size}) (partition 2 seeds)))

(def results (reduce (fn [seeds ranges]
                       (flatten (map (fn [s] (map-number-range s ranges)) seeds)))
                     seed-ranges
                     all-ranges))

(reduce min (map :start results))
