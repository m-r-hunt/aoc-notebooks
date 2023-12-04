;; # 2023 Day 4

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d4
  (:require [aoc-notebooks.input :as input]))

;; [Problem](https://adventofcode.com/2023/day/4)

;; ## Part 1

;; Grabbing numbers and tokens from input is enough since the divider will show up as a `:|` keyword (which is valid in Clojure, it's very flexible about allowed symbols and keywords).

(def input (input/numbers-and-tokens 2023 4))

;; Parsing out an individual line is fairly straightforward. We convert the card number into 0-based index for use later.

(defn parse
  [line-seq]
  (let [nums (drop 2 line-seq)
        [ours winning] (split-with #(not= % :|) nums)]
    {:index (dec (second line-seq)) 
     :ours (vec ours) 
     :winning (set (rest winning))}))

(def cards (mapv parse input))

;; Getting the number of our numbers that match the winning numbers is straightforward and the basis for the rest of the solution.

(defn matches
  [{:keys [ours winning]}]
  (count (filter winning ours)))

(matches (first cards))

;; Scoring a card is then easy.
;; Bitshifting is a quick and easy way to calculate the score, but we have to be careful not to try and shift by `-1`.

(defn score
  [card]
  (let [matches (matches card)]
    (if (> matches 0)
      (bit-shift-left 1 (dec matches))
      0)))

(score (first cards))

;; Finally sum up for the answer.

(reduce + (map score cards))

;; ## Part 2

;; This is a fairly imperative approach.
;; Using `reduce`, loop through all cards, with a count of all the cards we have as the reduced value.
;; In each iteration, we just look at the matches and add the number of cards we're looking at currently on to the next `n` cards.
;; This is a single walk through the data and should be pretty efficient.

(def counts (reduce (fn [counts {:keys [index] :as card}]
                      (let [copies (matches card)
                            this-card (get counts index)]
                        (reduce (fn [counts index] (update counts index + this-card))
                                counts
                                (range (inc index) (+ index copies 1)))))
                    (into [] (repeat (count cards) 1))
                    cards))

;; And sum up again for the final total count.

(reduce + counts)
