;; # 2023 Day 2

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d2
  (:require [aoc-notebooks.input :as input]))

;; [Problem](https://adventofcode.com/2023/day/2)

;; ## Part 1

;; We grab the input as numbers and tokens.
;; This throws away some information about how the turns of each game were seperated, but it turns out we won't need that (spoilers)

(def input (input/numbers-and-tokens 2023 2))

;; Do a bit more processing on each line to pull out the game id and data as [colour number] pairs and shove it into a map

(defn munge-line
  [line]
  {:id (second line) :data (->> line
                                (drop 2)
                                (partition 2))})

(def games (mapv munge-line input))

;; In `turn-valid` a "turn" is just one piece of information like "3 blue".
;; Just checking against the arbitrary limits given in the problem.

(defn turn-valid
  [[count colour]]
  (case colour
    :red (<= count 12)
    :blue (<= count 14)
    :green (<= count 13)))

;; A game is valid if all turns in it are valid.

(defn game-valid
  [game]
  (every? turn-valid game))

(game-valid (get-in games [0 :data]))
(game-valid (get-in games [3 :data]))

;; Finally sum up ids of valid games to get the answer.

(reduce + (map #(if (game-valid (:data %)) (:id %) 0) games))

;; ## Part 2

;; For part 2, we have not been punished for lazy input parsing.

;; Calculate the minimum required for a game by just looking at each turn to see if we need more marbles.

(defn min-required
  [game]
  (reduce #(update %1 (second %2) max (first %2)) {:red 0 :green 0 :blue 0} game))

(min-required (get-in games [0 :data]))
(min-required (get-in games [1 :data]))
(min-required (get-in games [2 :data]))

;; Then defining the "power" is simple.

(defn power
  [counts]
  (reduce #(* %1 (second %2)) 1 counts))

(power (min-required (get-in games [0 :data])))

;; And finally sum them up to finish. Pretty straightforward compared to yesterday.

(reduce + (map (comp power min-required :data) games))
