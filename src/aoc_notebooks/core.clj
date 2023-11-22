(ns aoc-notebooks.core
	 (:require [nextjournal.clerk]))

;; 123
;; More text
(defn main-
  "I don't do a whole lot."
  []
  (nextjournal.clerk/build! {:paths ["src/solutions/*"]})
  (nextjournal.clerk/serve! {:browse true :watch-paths ["src"]}))

(defn anc
		[y]
		(+ y 44))
