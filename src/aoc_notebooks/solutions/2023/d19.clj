;; # 2023 Day 19

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d19
  (:require [aoc-notebooks.input :as input]
            [clojure.string :as str]))

;; [Problem](https://adventofcode.com/2023/day/19)

;; ## Part 1

(def input (input/line-groups 2023 19))

(def instrs-raw (input/split-lines (first input)))
(println (apply str (interpose "\",\n\"" instrs-raw)))

(def data-raw (input/split-lines (second input)))

(defn parse-one
  [s]
  (let [op (first (filter #{\> \<} s))
        [id val dest] (str/split s #"[<>:]")]
    {:op op :id id :val (Long/parseLong val) :dest dest}))

(parse-one "a<2006:qkq")

(defn parse
  [s]
  (let [[name ops] (str/split s #"[{}]")
        ops-split (str/split ops #",")
        parsed-ops (map parse-one (drop-last ops-split))
        default (last ops-split)]
    {:name name :ops parsed-ops :default default}))

(parse (first instrs-raw))

(def instrs (->> (mapv parse instrs-raw)
                (map (fn [i] [(:name i) i]))
                (into {})))

(defn parse-data
  [s]
  (into {} (map (fn [[c v]] [c (Long/parseLong v)]) (apply hash-map (rest (str/split s #"[{},=]"))))))

(parse-data (first data-raw))

(def data (mapv parse-data data-raw))

(defn check-instr
  [{:keys [dest id op val]} p]
  (case op
    \> (if (> (get p id) val) dest nil)
    \< (if (< (get p id) val) dest nil)))

(defn process
  [p ikey]
  (let [{:keys [ops default]} (instrs ikey)
        dest (or (first (filter (comp not nil?) (map check-instr ops (repeat p))))
                 default)]
    (cond
      (= "A" dest) (reduce + (vals p))
      (= "R" dest) 0
      :else (process p dest))))

(reduce + (map process data (repeat "in")))

;; ## Part 2

(defn valid-range
  [{:keys [min max]}]
  (< min max))

#_(defn try-valid
  [ranges ikey]
  (let [{:keys [ops default]} (instrs ikey)
        op-ranges (map try-op ops (repeat p))]))
