(ns aoc-notebooks.core
  (:require [nextjournal.clerk]))

;; 123
;; More text
(defn main-
  "I don't do a whole lot."
  []
  (nextjournal.clerk/build! {:paths ["src/aoc_notebooks/solutions/**"] :out-path "docs"})
  (nextjournal.clerk/serve! {:browse true :watch-paths ["src"]}))
