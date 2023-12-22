;; # 2023 Day 18

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d18
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/18)

;; ## Part 1

(def input (binding [input/*token-re* #"[ \t,\/\\':;\"~{}]+"] (input/numbers-and-tokens 2023 18)))

(defn dig
  [{:keys [data] [px py] :pos :as grid} [dir dist col]]
  (let [[dx dy] (case dir
                  :r [1 0]
                  :l [-1 0]
                  :u [0 -1]
                  :d [0 1])]
    (assoc (reduce (fn [d n] (assoc-in d [:data [(+ px (* dx n)) (+ py (* dy n))]] col))
                   grid
                   (range 1 (inc dist)))
           :pos [(+ px (* dx dist)) (+ py (* dy dist))])))

(dig {:data {[0 0] "#000000"} :pos [0 0]} [:r 6 "#123456"])

(def dig-result (reduce dig {:data {[0 0] "#000000"} :pos [0 0]} input))

(def dig-result-grid (assoc dig-result 
                            :width (inc (apply max (map #(get-in % [0 0]) (:data dig-result))))
                            :height (inc (apply max (map #(get-in % [0 1]) (:data dig-result))))))

(input/draw-grid dig-result-grid :filter (fn [c d] \#) :default \.)

(def fill-start [1 1])

(def area (loop [to-check [fill-start] checked #{} area #{fill-start}]
            (if (seq to-check)
              (let [checking (peek to-check)
                    [cx cy] checking
                    neighbours (if (and (not (checked checking)) (not (get-in dig-result [:data checking])))
                                 [[(inc cx) cy] [(dec cx) cy] [cx (inc cy)] [cx (dec cy)]]
                                 [])
                    area (if (not (get-in dig-result [:data checking]))
                           (conj area checking)
                           area)
                    checked (conj checked checking)]
                (recur (apply conj (pop to-check) neighbours) checked area))
              area)))

(+ (count area)
   (count (:data dig-result)))

;; ## Part 2
