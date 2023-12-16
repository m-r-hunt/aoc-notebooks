;; # 2023 Day 13

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d13
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/13)

;; ## Part 1

(def input (input/line-groups 2023 13))

(def input-rows (mapv #(mapv vec (input/split-lines %)) input))

(defn to-cols
  [rows]
  (apply map vector rows))

(to-cols (input-rows 0))

(defn is-mirror-index
  [rows index]
  (let [[before after] (split-at index rows)
        before (reverse before)]
    (if (every? true? (map = before after))
      index)))

(defn find-mirror-rows
  [rows]
  (let [index-range (range 1 (count rows))]
    (first (filter identity (map #(is-mirror-index rows %) index-range)))))

(find-mirror-rows (input-rows 0))
(find-mirror-rows (input-rows 1))

(defn find-mirror
 [rows]
 (if-let [id (find-mirror-rows rows)]
   (* 100 id)
   (find-mirror-rows (to-cols rows))))

(find-mirror (input-rows 0))
(find-mirror (input-rows 1))

(reduce + (map find-mirror input-rows))

;; ## Part 2

(defn find-mirror-rows-ignore
  [rows ignore]
  (let [index-range (range 1 (count rows))]
    (first (filter identity (map #(if (and (is-mirror-index rows %) (not= % ignore)) (is-mirror-index rows %) nil) index-range)))))

(defn find-mirror-ignore
 [rows ignore]
 (if-let [id (find-mirror-rows-ignore rows (/ ignore 100))]
   (* 100 id)
   (find-mirror-rows-ignore (to-cols rows) ignore)))

(defn coords
  [y x]
  (apply concat (map (fn[y] (map vector (repeat y) (range x))) (range y))))

(coords 4 2)

(def tg (input-rows 0))

(defn all-smudges
  [rows]
  (map #(update-in rows % (fn [c] (if (= c \#) \. \#))) (coords (count rows) (count (first rows)))))

(defn smudged-val
  [rows]
  (let [orig (find-mirror rows)]
    (first (filter #(and (not (nil? %)) (not= % orig)) (map find-mirror-ignore (all-smudges rows) (repeat orig))))))

(def smudged (update-in tg [5 8] (fn [c] (if (= c \#) \. \#))))
(= smudged tg)
(find-mirror tg)
(find-mirror smudged)

(smudged-val tg)

(map-indexed vector (map smudged-val input-rows))
(filter #(nil? (second %)) (map-indexed vector (map smudged-val input-rows)))

(reduce + (map smudged-val input-rows))
