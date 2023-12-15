;; # 2023 Day 15

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d15
  (:require [aoc-notebooks.input :as input]
            [clojure.string :as str]))

;; [Problem](https://adventofcode.com/2023/day/15)

;; ## Part 1

(def input (input/split-lines 2023 15))

(def instrs (flatten (map #(str/split % #",") input)))

(int \c)

(defn HASH
  [s]
  (reduce #(mod (*(+ %1 (int %2)) 17) 256) 0 (map int s)))

(HASH "HASH")

(reduce + (map HASH instrs))

;; ## Part 2

(def boxes (mapv (fn [id] {:id id :lenses []}) (range 0 256)))

(defn parse-instr
  [instr]
  (if (re-find #"=" instr )
    (let [[label fl] (str/split instr #"=")]
      {:op :equal :label label :fl (Long/parseLong fl) :box-n (HASH label)})
    (let [label (apply str (drop-last instr))]
      {:op :dash :label label :box-n (HASH label)})))

(parse-instr (nth instrs 0))
(parse-instr (nth instrs 1))

(def parsed (mapv parse-instr instrs))

(defn run-instr
  [boxes {:keys [op label fl box-n]}]
  (let [already-present (some? (seq (filter #(= (:label %) label) (get-in boxes [box-n :lenses]))))]
    (case op
      :equal (if already-present
               (update-in boxes [box-n :lenses] #(mapv (fn [l] (if (= (:label l) label) (assoc l :fl fl) l)) %))
               (do (println "hi") (update-in boxes [box-n :lenses] conj {:label label :fl fl})))
      :dash (update-in boxes [box-n :lenses] #(into [] (filter (fn [l] (not= (:label l)
                                                                             label))
                                                               %))))))

(def result (reduce run-instr boxes parsed))

(defn power
  [{:keys [id lenses]}] 
  (reduce + (map-indexed (fn [slot-n {:keys [fl]}] (* (inc id) (inc slot-n) fl)) lenses)))

(power (first result))
(power (result 3))

(reduce + (map power result))
