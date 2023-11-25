;; # 2022 Day 7

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2022.d7
  (:require [aoc-notebooks.input :as input]
            [clojure.walk :as walk]))

;; [Problem](https://adventofcode.com/2023/day/7)

;; ## Part 1

(def input (binding [input/*token-re* #"[ \t,\\':;\"#~{}]+"] 
             (input/numbers-and-tokens 2022 7)))

(def tree (first (reduce (fn [[tree path] [first second & [third]]]
                           (if (= first :$)
                             (case second
                               :cd (case third
                                     :.. [tree (vec (drop-last path)) 0]
                                     :/ [tree [:/] 0]
                                     [tree (conj path third) 0])
                               :ls [tree path])
                             (if (= first :dir)
                               [tree path]
                               [(assoc-in tree (conj path second) first) path]))) 
                         [{} [:/]] 
                         input)))

(def sized-tree (walk/postwalk (fn [f]
                           (println f)
                           (if (map? f)
                             (assoc f :size (reduce (fn [total [_k v]] (+ total (if (map? v) (:size v) v))) 0 f))
                             f)) tree))

(reduce 
 (fn [total m] (+ total (if (and (map? m) (<= (:size m) 100000)) (:size m) 0))) 
 0 
 (tree-seq map? vals sized-tree))

;; ## Part 2

(def unused (- 70000000 (:size sized-tree)))

(def delete-at-least (- 30000000 unused))

(def good-dirs (filter #(and (map? %) (>= (:size %) delete-at-least)) (tree-seq map? vals sized-tree)))

(def good-dir-sizes (map #(:size %) good-dirs))

(apply min good-dir-sizes)
