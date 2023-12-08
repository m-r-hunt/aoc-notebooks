;; # 2023 Day 3

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d3
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/3)

;; ## Part 1

;; Let's grab the input as a grid.

(def input (input/grid 2023 3))

(input/draw-grid input)

;; Firstly we'll grab all the numbers in the grid, along with a list of the coordinates they occupied.
;; This is a bit of a nasty piece of imperative double-looping, could definitely be  improved.

(def nums (loop [y 0 nums []]
            (if (< y (:height input))
              (let [nums (loop [x 0 nums nums]
                           (if (< x (:width input))
                             (if (utils/digit? (get-in input [:data [x y]]))
                               (let [line-xs (range x (:width input))
                                     valid-xs (take-while #(utils/digit? (get-in input [:data [% y]])) line-xs)
                                     num (Integer/parseInt (apply str (map #(get-in input [:data [% y]]) valid-xs)))]
                                 (recur (inc (last valid-xs)) (conj nums {:value num :coords (mapv #(vector % y) valid-xs)})))
                               (recur (inc x) nums))
                             nums))]
                (recur (inc y) nums))
              nums)))

;; To find machine parts, we need to look at neighbouring cells for each number.
;; This is a "smart" diagonal neighbours function which also bounds checks and only returns valid coordinates in our input grid.

(defn neighbours
  [[x y]]
  (filter (fn [[x y]] (and (< -1 x (:width input))
                           (< -1 y (:height input))))
          [[(dec x) (dec y)]
           [(dec x) y]
           [(dec x) (inc y)]
           [x (dec y)]
           [x (inc y)]
           [(inc x) (dec y)]
           [(inc x) y]
           [(inc x) (inc y)]]))

(neighbours [0 0])
(neighbours [5 5])

;; Next we can check whether a number has a symbol neighbouring it.
;; We look at all neighbouring coordinates, check them for non-number and non-. entries, and then check if we got at least one hit.

(defn has-symbol-neighbour
  [{:keys [coords]}]
  (some identity (map #(not (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \.} (get-in input [:data %]))) (mapcat neighbours coords))))

(has-symbol-neighbour (first nums))
(has-symbol-neighbour (second nums))

;; Then we can filter down to valid numbers with a symbol neighbour, and sum up the resulting values.

(def valid-nums (filter has-symbol-neighbour nums))

(reduce + (map :value valid-nums))

;; ## Part 2

;; Now we need to shift gears and look at the stars (potential gears), although having the valid numbers parsed and ready will come in handy.

;; First just lookup all the stars.

(def stars (map first (filter #(= (second %) \*) (:data input))))

;; Next we need to figure out which are gears.
;; This is a helper which checks if a coordinate is in a valid number, returns the number map if so or nil otherwise.

(defn is-in-num
  [c]
  (first (filter #(some identity (map (fn [cc] (= cc c)) (:coords %))) nums)))

(is-in-num [0 0])
(is-in-num [0 1])

;; Then we can figure out which stars are gears by looking at the star's neighbours, finding all the neighbouring numbers, and checking if there are exactly 2.
;; This returns a set of the neighbouring numbers or nil otherwise, so we can use keep to filter and map in one go.

(defn is-gear
  [c]
  (let [neighbours (neighbours c)
        num-neighbours (set (keep is-in-num neighbours))]
    (if (= (count num-neighbours) 2)
      num-neighbours
      nil)))

(is-gear [3 1])
(is-gear [3 4])

(def gears (keep is-gear stars))

;; Then at last we just go through our gears, calculating the gear ratios and summing.

(reduce + (map #(apply * (map :value %)) gears))
