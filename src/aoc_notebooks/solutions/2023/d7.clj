;; # 2023 Day 7

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d7
  (:require [aoc-notebooks.input :as input]
            [clojure.string :as str]))

;; [Problem](https://adventofcode.com/2023/day/7)

;; ## Part 1

(def input (binding [input/*token-fn* identity] (input/tokens 2023 7)))

(def hands (->> input
                (map (fn [h] (update h 1 #(Long/parseLong %))))
                (mapv (fn [[hand bet]] {:hand hand :bet bet}))))

(defn to-sortable
  [hand rank]
  (let [munged (-> hand
                   (str/replace "2" "a")
                   (str/replace "3" "b")
                   (str/replace "4" "c")
                   (str/replace "5" "d")
                   (str/replace "6" "e")
                   (str/replace "7" "f")
                   (str/replace "8" "g")
                   (str/replace "9" "h")
                   (str/replace "T" "i")
                   (str/replace "J" "j")
                   (str/replace "Q" "k")
                   (str/replace "K" "l")
                   (str/replace "A" "m"))]
    (str rank munged)))

  (to-sortable (get-in hands [0 :hand]) 3)

(defn rank
  [hand]
  (let [elems (set hand)
        counts (sort (map #(count (filter #{%} hand)) elems))
        highest (last counts)
        next-highest (first (take-last 2 counts))]
    (cond 
      (= highest 5) 7
      (= highest  4) 6
      (and (= highest 3) (= next-highest 2)) 5
      (= highest  3) 4
      (and (= highest  2) (= next-highest 2)) 3
      (= highest 2) 2
      (= highest  1) 1)))
  
(rank (get-in hands [0 :hand]))
(mapv #(assoc % :rank (rank (:hand %))) hands)

(def sorted-hands (->> hands
                       (map #(assoc % :rank (rank (:hand %))))
                       (map #(assoc % :sort (to-sortable (:hand %) (:rank %))))
                       (sort-by :sort)))
  
(reduce + (map-indexed #(* (inc %1) (:bet %2)) sorted-hands))

;; ## Part 2

(defn to-sortable2
  [hand rank]
  (let [munged (-> hand
                   (str/replace "J" "a")
                   (str/replace "2" "b")
                   (str/replace "3" "c")
                   (str/replace "4" "d")
                   (str/replace "5" "e")
                   (str/replace "6" "f")
                   (str/replace "7" "g")
                   (str/replace "8" "h")
                   (str/replace "9" "i")
                   (str/replace "T" "j")
                   (str/replace "Q" "k")
                   (str/replace "K" "l")
                   (str/replace "A" "m"))]
    (str rank munged)))

(to-sortable (get-in hands [0 :hand]) 3)

(defn rank2
  [hand]
  (let [elems (set hand)
        jokers (count (filter #{\J} hand))
        counts (sort (map #(count (filter (fn [c] ( and (not= \J c) (#{%} c))) hand)) elems))
        highest (+ jokers (last counts))
        next-highest (first (take-last 2 counts))]
    (cond 
      (= highest 5) 7
      (= highest  4) 6
      (and (= highest 3) (= next-highest 2)) 5
      (= highest  3) 4
      (and (= highest  2) (= next-highest 2)) 3
      (= highest 2) 2
      (= highest  1) 1)))
  
(rank2 (get-in hands [0 :hand]))

(def sorted-hands2 (->> hands
                       (map #(assoc % :rank (rank2 (:hand %))))
                       (map #(assoc % :sort (to-sortable2 (:hand %) (:rank %))))
                       (sort-by :sort)))
  
(reduce + (map-indexed #(* (inc %1) (:bet %2)) sorted-hands2))

