;; # 2023 Day 10

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d10
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/10)

;; ## Part 1

(def input (input/grid 2023 10))

(comment (input/draw-grid input))

;; First go find the start point, and infer the start direction by looking at neighbouring tiles.
;; There are 2 possible start points, just pick one, it doesn't matter.

(def start (first (first (filter #(= \S (second %)) (:data input)))))

(def start-dir 
  (let [[x y] start
        n [x (dec y)]
        s [x (inc y)]
        e [(inc x) y]
        w [(dec x) y]]
    (cond 
      (#{\- \7 \J} (get-in input [:data e])) :east
      (#{\- \F \L} (get-in input [:data w])) :west
      (#{\| \7 \F} (get-in input [:data n])) :north
      (#{\| \L \J} (get-in input [:data s])) :south)))

;; With the start established, we can follow the pipe by walking one tile at a time, changing direction based on the tile we found and the direction we came from.
;; Big nasty case statement to handle that but what can you do?

;; Finish looping when we get back to the start, and save the path that we built up.

(def path (loop [[x y] start dir start-dir path [start]]
            (let [next (case dir
                         :north [x (dec y)]
                         :south [x (inc y)]
                         :east [(inc x) y]
                         :west [(dec x) y])
                  next-t (get-in input [:data next])]
              (if (= \S next-t)
                path
                (let [next-dir (case [dir next-t]
                                 [:north \|] :north
                                 [:north \7] :west
                                 [:north \F] :east
                                 [:east \-] :east
                                 [:east \7] :south
                                 [:east \J] :north
                                 [:west \-] :west
                                 [:west \F] :south
                                 [:west \L] :north
                                 [:south \|] :south
                                 [:south \J] :west
                                 [:south \L] :east)]
                  (recur next next-dir (conj path next)))))))

;; The answer for part 2 is then half the total length of the path, for the furthest point from the start (remembering it's a loop so the end of our path is back at the beginning).
;; No off-by-one since we didn't count the start tile twice.

(def answer (/ (count path) 2))

(path answer)

;; ## Part 2

;; For part 2, we'll want to consider the grid with the starting tile actually set to the correct tile rather than `\S`.
;; Hack: hardcoded the correct tile, the logic to do it properly would be annoying to write.

(def s-type \-)

(def fixed-input (assoc-in input [:data start] s-type))

;; Then we need a function to find enclosed tiles.
;; A tile is enclosed if:
;; * It's not in the path-set
;; * If you shoot a ray from any point inside the tile in any direction (I used north) to "infinity", it crosses the path an odd number of times

;; There's probably a topological theorem for point 2, my topology is too rusty to name it. I used my intuition and didn't worry too much.

;; For "infinity", just shooting to the edge of the grid is enough since we know the path is contained in the grid.

;; We imagine shooting a ray from slightly off-center in the tile. This makes it clear that it does not intersect with vertical pipes and only intersects with corner pipes that turn in the relevant direction.
;; Shooting down dead center gets confusing because we'd have to account for travelling "along" a pipe as one intersection.
;; The choice of which direction to offset is arbitrary, you should get the same final answer either way.

(def path-set (apply hash-set path))

(defn enclosed
  [[x y :as c]]
  (if (path-set c)
    false
    (let [walk-n (map vector (repeat x) (range 0 y))
          crossings (filter #(and (path-set %) 
                                  (#{\- \J \7} (get-in fixed-input [:data %]))) 
                            walk-n)]
      (= (mod (count crossings) 2) 1))))

(enclosed [12 5])

;; With that function written, we just count up enclosed tiles.

(count (filter enclosed (keys (:data input))))

(input/draw-grid input :filter (fn [c d] (if (enclosed c) \I (if (path-set c) \P d))))
