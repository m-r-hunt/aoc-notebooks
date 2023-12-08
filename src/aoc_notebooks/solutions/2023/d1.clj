;; # 2023 Day 1

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d1
  (:require [aoc-notebooks.input :as input]
            [aoc-notebooks.utils :as utils]))

;; [Problem](https://adventofcode.com/2023/day/1)

;; ## Part 1

;; Rather than any fancy input parsing, we just get lines from the input to work with directly

(def input (input/split-lines 2023 1))


;; Calculating the calibration value for a line is then simply a case of pulling out first and last digits, and combining them appropriately.

(defn calib-value
  [s]
  (let [digits (filter utils/digit? s)
        first (Integer/parseInt (str (first digits)))
        last (Integer/parseInt (str (last digits)))]
    (+ (* 10 first) last)))

(calib-value "1abc2")

;; Finally, just calculate the calibration value for each line and sum them up.

(reduce + (map calib-value input))

;; ## Part 2

;; For part 2 we have to switch gears a bit.
;; We can't preprocess the string like we did in part 1, because there may be multiple overlapping digit strings like "twone".
;; Instead we'll have to actually walk forwards and backwards through the string looking for the first digit-or-digit-string.

;; First we'll define a function that looks at a string, and returns a digit (as a number) if it *starts with* a digit or a digit string.

(defn find-digit
  [s]
  (cond
    (re-matches #"^one.*" s) 1
    (re-matches #"^two.*" s) 2
    (re-matches #"^three.*" s) 3
    (re-matches #"^four.*" s) 4
    (re-matches #"^five.*" s) 5
    (re-matches #"^six.*" s) 6
    (re-matches #"^seven.*" s) 7
    (re-matches #"^eight.*" s) 8
    (re-matches #"^nine.*" s) 9
    (re-matches #"^ten.*" s) 0
    (re-matches #"^[0-9].*" s) (Integer/parseInt (str (first s)))
    :else nil))

(find-digit "one")
(find-digit "1")
(find-digit "oneasdf")
(find-digit "1gaweeg")
(find-digit "a1")

;; Then to find the first digit in a string, we can walk forwards through the string, using our find-digit function until it returns a non-nil number.
;; No error checking: if there were no digits this would loop forever or crash or something.

(defn first-digit
  [s]
  (loop [s s]
    (if-let [d (find-digit s)]
      d
      (recur (apply str (rest s))))))

(first-digit "2")
(first-digit "a1")
(first-digit "twone")

;; Similarly to find the last digit we walk backwards through the string, looking at progressively larger suffixes until one is a hit by find-digit.

(defn last-digit
  [s]
  (loop [i 1]
    (if-let [d (find-digit (apply str (take-last i s)))]
      d
      (recur (inc i)))))
(last-digit "twone")

;; With those functions written, it's then a repeat of part 1 to find all calibration values and sum them up.

(defn calib-value2
  [s]
  (let [first (first-digit s)
        last (last-digit s)]
    (+ (* 10 first) last)))


(reduce + (map calib-value2 input))
