(ns aoc-notebooks.input
  (:require [clojure.string :as str]
            [nextjournal.clerk :as clerk]))

;; Input helper library designed for Advent of Code
;; Designed to cover all the common cases with minimal effort/typing.
;; Some functions are intentionally loose and do not strictly parse input to allow them to cover many cases as hack jobs.
;;
;; Most functions have 3 modes: 
;; * (helper year day) will load the file input/year/dday.txt and run the function on it
;; * (helper year day testcase) will load the file input/year/dday_testcase.txt and run the function on it
;; * (helper string) will just run the function against a string
;;
;; `(read-input year day)` does the data loading, can be called directly if that's helpful.

(defn read-input
  ([year day]
   (slurp (str "input/" year "/d" day ".txt")))
  ([year day testcase]
   (slurp (str "input/" year "/d" day "_" testcase ".txt"))))

(read-input 2022 1)

;; `split-lines` splits input into a `vec` of individual line strings.
;; There may be empty strings present if the source has blank lines.

(defn split-lines
  ([input]
   (str/split input #"\r?\n"))
  ([year day]
   (split-lines (read-input year day)))
  ([year day testcase]
   (split-lines (read-input year day testcase))))

(split-lines 2022 1)

;; `split-tokens` attempts to split a string into "tokens", seperated by whitespace or common punctuation characters.

;; The dynamic variable `*token-re*` determines what splits a token. Rebind it if you need to use a different char set.

(def ^:dynamic *token-re* #"[ \t,\/\\':;\"#~{}]+")

(defn split-tokens
  [line]
  (str/split line *token-re*))

;; `numbers` tries to parse out all the numbers in a file. It returns a `vec` of line `vec`s of numbers.
;; It splits by tokens within each line.

(defn numbers
  ([input]
   (as-> input input
     (split-lines input)
     (map split-tokens input)
     (map (fn [line] (into [] (map #(Long/parseLong %) line))) input)
     (into [] input)))
  ([year day]
   (numbers (read-input year day)))
  ([year day testcase]
   (numbers (read-input year day testcase))))

(numbers "1 2 3\n3,4,5")

;; `line-groups` splits a string by blank lines. Returns a vec of (unsplit/unprocessed) strings for each group seperated by blank lines.

(defn line-groups
  ([input]
   (str/split input #"\r?\n\r?\n"))
  ([year day]
   (line-groups (read-input year day)))
  ([year day testcase]
   (line-groups (read-input year day testcase))))

(line-groups 2022 1)

(map numbers (line-groups 2022 1))

;; `tokens` (TODO: change name?) works similarly to `numbers` except it takes each token, lowercases it and turns it into a keyword

(def ^:dynamic *token-fn* (comp keyword str/lower-case))

(defn tokens
  ([input]
   (as-> input input
     (split-lines input)
     (map split-tokens input)
     (map (fn [line] (into [] (map *token-fn* line))) input)
     (into [] input)))
  ([year day]
   (tokens (read-input year day)))
  ([year day testcase]
   (tokens (read-input year day testcase))))

(tokens 2022 2)

;; `numbers-and-tokens` combines `numbers` and `token`, splitting the input the same way but parsing out numbers as numbers and everything else as a lowercased keyword.

(defn numbers-and-tokens
  ([input]
   (as-> input input
     (split-lines input)
     (map split-tokens input)
     (map (fn [line] (into [] (map #(if (re-matches #"^[0-9].*" %)
                                      (Long/parseLong %)
                                      (*token-fn* %))
                                   line)))
          input)
     (into [] input)))
  ([year day]
   (numbers-and-tokens (read-input year day)))
  ([year day testcase]
   (numbers-and-tokens (read-input year day testcase))))

(numbers-and-tokens 2022 7)

;; `grid` parses the input as a dense grid of individual characters. It returns a map `{:width :height :data}`.
;;
;; `:width` is the highest `x` coord `+ 1`, `:height` is the highest `y` coord `+ 1`.
;;
;; `:data` is a map from `[x y]` to `char`.
;; If the input data is not rectangular, some entries may be missing from `:data`

(defn grid
  ([input]
   (let [data (as-> input input
                (split-lines input)
                (map-indexed (fn [y line] (map-indexed (fn [x c] [[x y] c]) line)) input)
                (apply concat input)
                (apply concat input)
                (apply hash-map input))
         height (inc (reduce max 0 (map (fn [[[_x y] _c]] y) data)))
         width (inc (reduce max 0 (map (fn [[[x _y] _c]] x) data)))]
     {:data data :height height :width width}))
  ([year day]
   (grid (read-input year day)))
  ([year day testcase]
   (grid (read-input year day testcase))))

(def test-grid (grid 2022 8))
(get-in test-grid [:data [10 10]])
(get-in test-grid [:data [11 10]])
(get-in test-grid [:data [10 11]])

;; draw-grid formats a grid (i.e. one returned by `grid` or structured the same way) for display in a clerk notebook.
;; Supports keyword arguments for options:
;; * `:default \¬` - character to use for unmapped parts of the grid for any non-rectangular grid
;; * `:filter (fn [c d] d)` - filtering function to transform grid before display. Takes coordinate and existing grid value and returns value that should be displayed

(defn draw-grid
  ([{:keys [height width data]} & {:keys [default filter]
                                   :or {default \¬ filter (fn [_c d] d)}}]
   (let [xs (range 0 width)
         ys (range 0 height)
         filtered (into {} (map (fn [[c d]] [c (filter c d)]) data))]
     (clerk/html [:div.font-mono.whitespace-pre (apply str (flatten (map (fn [y] (conj (map (fn [x] (or (filtered [x y]) default)) xs) "\n")) ys)))]))))

(draw-grid test-grid)
