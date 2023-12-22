;; # 2023 Day 17

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d17
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]
            [clojure.data.priority-map :as pmap]))

;; [Problem](https://adventofcode.com/2023/day/17)

;; ## Part 1

(def input (input/grid 2023 17))

(input/draw-grid input)

(defn neighbours
  [{[px py] :pos [dx dy :as dir] :dir straights :straights}]
  (filter #(and (get-in input [:data (:pos %)])
                (< (:straights %) 3))
          [{:pos [(+ px dx) (+ py dy)] :dir dir :straights (inc straights)}
           {:pos [(+ px dy) (- py dx)] :dir [dy (- dx)] :straights 0}
           {:pos [(- px dy) (+ py dx)] :dir [(- dy) dx] :straights 0}]))

(neighbours {:pos [0 0] :dir [1 0] :straights 0})

(def dest [(dec (:width input)) (dec (:height input))])

#_(def result (loop [queue (pmap/priority-map {:pos [0 0] :dir [1 0] :straights 0} 0) 
                   came-from {{:pos [0 0] :dir [1 0] :straights 0} "start"} 
                   costs {{:pos [0 0] :dir [1 0] :straights 0} 0}]
              (let [[item _prio] (peek queue)
                    queue (pop queue)
                    neighbours (neighbours item)
                    neighbours (mapv (fn [n] [n 
                                              (+ (get costs item) (Long/parseLong (str (get-in input [:data (:pos n)]))))
                                              (+ (get costs item) 
                                                 (Long/parseLong (str (get-in input [:data (:pos n)]))) 
                                                 (abs (- (first dest) (first (:pos n))))
                                                 (abs (- (second dest) (second (:pos n)))))]) 
                                     neighbours)
                    neighbours (filter (fn [[n cost prio]] (or (not (contains? costs n))
                                                               (< cost (get costs n)))) neighbours)]
                [neighbours item queue]
                (if (= (:pos item) dest)
                  [item came-from costs]
                  (recur (reduce #(assoc %1 (first %2) (nth %2 2)) queue neighbours)
                         (reduce #(assoc %1 (first %2) item) came-from neighbours)
                         (reduce #(assoc %1 (first %2) (second %2)) costs neighbours))))))

#_(get (nth result 2) (first result))

;; ## Part 2


(defn neighbours2
  [{[px py] :pos [dx dy :as dir] :dir straights :straights}]
  (let [candidates (if (< straights 3)
                     [{:pos [(+ px dx) (+ py dy)] :dir dir :straights (inc straights)}]
                     [{:pos [(+ px dx) (+ py dy)] :dir dir :straights (inc straights)}
                      {:pos [(+ px dy) (- py dx)] :dir [dy (- dx)] :straights 0}
                      {:pos [(- px dy) (+ py dx)] :dir [(- dy) dx] :straights 0}])]
    (filter #(and (get-in input [:data (:pos %)])
                  (< (:straights %) 10))
            candidates)))


#_(def result2 (loop [queue (pmap/priority-map {:pos [0 0] :dir [0 1] :straights -1} 0)
                   came-from {{:pos [0 0] :dir [0 1] :straights -1} "start"}
                   costs {{:pos [0 0] :dir [0 1] :straights -1} 0}]
              (let [[item _prio] (peek queue)
                    queue (pop queue)
                    neighbours (neighbours2 item)
                    neighbours (mapv (fn [n] [n
                                              (+ (get costs item) (Long/parseLong (str (get-in input [:data (:pos n)]))))
                                              (+ (get costs item)
                                                 (Long/parseLong (str (get-in input [:data (:pos n)])))
                                                 (abs (- (first dest) (first (:pos n))))
                                                 (abs (- (second dest) (second (:pos n)))))])
                                     neighbours)
                    neighbours (filter (fn [[n cost prio]] (or (not (contains? costs n))
                                                               (< cost (get costs n)))) neighbours)]
                [neighbours item queue]
                (if (and (= (:pos item) dest) (>= (:straights item) 3))
                  [item came-from costs]
                  (recur (reduce #(assoc %1 (first %2) (nth %2 2)) queue neighbours)
                         (reduce #(assoc %1 (first %2) item) came-from neighbours)
                         (reduce #(assoc %1 (first %2) (second %2)) costs neighbours))))))

#_(get (nth result2 2) (first result2))

#_(def path (loop [path [(nth result2 0)]]
            (let [curr (last path)
                  came-from (nth result2 1)]
              (if (= curr "start")
                path
                (recur (conj path (get came-from curr)))))))

#_(def path-pos (into #{} (map :pos path)))

#_(input/draw-grid input :filter(fn [c d] (if (path-pos c) "#" d)))
