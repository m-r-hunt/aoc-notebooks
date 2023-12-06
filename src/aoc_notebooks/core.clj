(ns aoc-notebooks.core
  (:require [nextjournal.clerk]))

(defn main-
  "I don't do a whole lot."
  []
  #_(nextjournal.clerk/build! {:paths ["src/aoc_notebooks/solutions/**"] :out-path "docs"})
  (nextjournal.clerk/serve! {:browse true :watch-paths ["src"]}))
