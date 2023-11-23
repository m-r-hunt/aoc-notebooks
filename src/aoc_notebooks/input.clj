(ns aoc-notebooks.input
  (:require [clojure.string :as str]))

(defn read-input
  [year day]
  (slurp (str "input/" year "/d" day ".txt")))

(read-input 2022 1)

(defn numbers
  ([input]
   (as-> input input
     (str/split input #"\r?\n")
     (map #(str/split % #"[ \t,/\\':;\"#~{}]") input)
     (map (fn [line] (into [] (map #( Integer/parseInt %) line))) input)
     (into [] input)))
  ([year day]
   (numbers (read-input day year))))

(defn line-groups
  ([input]
   (str/split input #"\r?\n\r?\n"))
  ([year day]
   (line-groups (read-input year day))))

(map numbers (line-groups 2022 1))

(numbers "1 2 3\n3 4 5")
