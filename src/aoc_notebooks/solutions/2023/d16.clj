;; # 2023 Day 16

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d16
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/16)

;; ## Part 1

(def input (input/grid 2023 16))

(input/draw-grid input)

(defn simulate-beam
  [start]
  (loop [active [start]
                        seen #{}]
                   (if (= (count active) 0)
                     seen
                     (let [{:keys [pos dir]} (first active)
                           stepped {:pos [(+ (pos 0) (dir 0))
                                          (+ (pos 1) (dir 1))]
                                    :dir dir}
                           cur-tile (get-in input [:data (:pos stepped)])]
                       (case cur-tile
                         \. (if-not (seen stepped)
                              (recur (assoc active 0 stepped) (conj seen stepped))
                              (recur (into [] (rest active)) seen))
                         \\ (let [new-dir (case dir
                                            [1 0] [0 1]
                                            [-1 0] [0 -1]
                                            [0 1] [1 0]
                                            [0 -1] [-1 0])
                                  stepped (assoc stepped :dir new-dir)]
                              (if-not (seen stepped)
                                (recur (assoc active 0 stepped) (conj seen stepped))
                                (recur (into [] (rest active)) seen)))
                         \/ (let [new-dir (case dir
                                            [1 0] [0 -1]
                                            [-1 0] [0 1]
                                            [0 1] [-1 0]
                                            [0 -1] [1 0])
                                  stepped (assoc stepped :dir new-dir)]
                              (if-not (seen stepped)
                                (recur (assoc active 0 stepped) (conj seen stepped))
                                (recur (into [] (rest active)) seen)))
                         \- (if (#{[1 0] [-1 0]} dir)
                              (if-not (seen stepped)
                                (recur (assoc active 0 stepped) (conj seen stepped))
                                (recur (into [] (rest active)) seen))
                              (let [nb1 (assoc stepped :dir [1 0])
                                    nb2 (assoc stepped :dir [-1 0])]
                                (recur (conj (into [] (rest active)) nb1 nb2) (conj seen nb1 nb2))))
                         \| (if (#{[0 1] [0 -1]} dir)
                              (if-not (seen stepped)
                                (recur (assoc active 0 stepped) (conj seen stepped))
                                (recur (into [] (rest active)) seen))
                              (let [nb1 (assoc stepped :dir [0 1])
                                    nb2 (assoc stepped :dir [0 -1])]
                                (recur (conj (into [] (rest active)) nb1 nb2) (conj seen nb1 nb2))))
                         nil (recur (into [] (rest active)) seen))))))

(def states-seen (simulate-beam {:pos [-1 0] :dir [1 0]}))

(def tiles-seen (into #{} (map :pos states-seen)))

(input/draw-grid input :filter (fn [c d] (if (tiles-seen c) "#" ".")))

(count tiles-seen)

;; ## Part 2

(def entrances (concat (map (fn [y] {:pos [-1 y] :dir [1 0]}) (range 0 (:height input)))
                       (map (fn [y] {:pos [(:width input) y] :dir [-1 0]}) (range 0 (:height input)))
                       (map (fn [x] {:pos [x -1] :dir [0 1]}) (range 0 (:width input)))
                       (map (fn [x] {:pos [x (:height input)] :dir [0 -1]}) (range 0 (:width input)))))

(apply max (map count (map #(into #{} (map :pos %)) (map simulate-beam entrances))))

