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
  {:name name :ranges (vec (sort-by :src (map make-range ranges)))})

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
  (let [ranges-start (get-in ranges [0 :src])]
    (if (< start ranges-start)
      (if (> (+ start range-size) ranges-start)
        (conj (map-number-range {:start ranges-start :range-size (- range-size (- ranges-start start))} ranges) 
              {:start start :range-size (- ranges-start start)})
        [orig])
      (let [[lower-ranges higher-ranges] (split-with #(<= (:src %) start) ranges)
            last-range (last lower-ranges)
            last-range-end (+ (:src last-range) (:size last-range))
            next-range (first higher-ranges)
            next-range-start (if next-range (:src next-range) Long/MAX_VALUE)
            last-range-comp (if (< start last-range-end)
                              [{:start (+ (:dest last-range) (- start (:src last-range))) :range-size (min range-size (- last-range-end start))}]
                              [])
            empty-space-comp (if (and (>= (+ start range-size) last-range-end) (> next-range-start last-range-end))
                               (let [empty-space-end (min (+ start range-size) next-range-start)
                                     empty-space-start (max start last-range-end)]
                                 [{:start empty-space-start :range-size (- empty-space-end empty-space-start)}])
                               [])
            next-range-comp (if (and next-range (>= (+ start range-size) next-range-start))
                              (map-number-range {:start next-range-start :range-size (- (+ start range-size) next-range-start)} ranges)
                              [])]
        (vec (concat last-range-comp empty-space-comp next-range-comp))))))

(def seed-ranges (mapv (fn [[start range-size]] {:start start :range-size range-size}) (partition 2 seeds)))

(def s1 (flatten (map (fn [s] (map-number-range s (first all-ranges))) seed-ranges)))
(def s2 (flatten (map (fn [s] (map-number-range s (second all-ranges))) s1)))
(def s3 (flatten (map (fn [s] (map-number-range s (nth all-ranges 2))) s2)))

(def results (reduce (fn [seeds ranges]
                       (flatten (map (fn [s] (map-number-range s ranges)) seeds)))
                     seed-ranges
                     all-ranges))

(reduce min (map :start results))
