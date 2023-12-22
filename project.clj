(defproject aoc-notebooks "0.1.0-SNAPSHOT"
  :description "Advent of Code Clojure Notebooks"
  :url "https://mechtoast.com/aoc-notebooks"
  :license {:name "MIT"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [io.github.nextjournal/clerk "0.15.957"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :repl-options {:init-ns aoc-notebooks.core}
  :main aoc-notebooks.core/main-)
