;; # 2023 Day z

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.hod-2022.d1
  (:require [clojure.string :as str]
            [clojure.edn]))

;; [Problem](https://hanukkah.bluebird.sh/5783/1/)

;; ## Puzzle 1

(def input (-> (slurp (str "input/hod_2022/noahs-customers.csv"))
               (str/split #"\r?\n")))

(def headers (as-> (first input) input 
                 (str/split input #",")
                 (map keyword input)))


(defn make-data-map
  [line]
  (as-> line line
      (clojure.edn/read-string (str "[" line "]")) ; Hack: csv is close enough to edn that we can just surround it with [] and call read-string
      (zipmap headers line)))

(make-data-map (second input))

(def data (mapv make-data-map (rest input)))

(defn surname
  [row]
  (last (str/split (:name row) #" ")))

(surname (first data))

(def phone-keys {\a \2 \b \2 \c \2
                 \d \3 \e \3 \f \3
                 \g \4 \h \4 \i \4
                 \j \5 \k \5 \l \5
                 \m \6 \n \6 \o \6
                 \p \7 \q \7 \r \7 \s \7
                 \t \8 \u \8 \v \8
                 \w \9 \x \9 \y \9 \z \9})

(defn numberify
  [name]
  (map phone-keys (str/lower-case name)))

(numberify (surname (first data)))

(defn phone-seq
  [row]
  (filter #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} (seq (:phone row))))

(phone-seq (first data))

(def pi (first (filter #(= (phone-seq %) (numberify (surname %))) data)))

(:phone pi)

;; Puzzle 2

(defn initials
  [row]
  (apply str (map first (str/split (:name row) #" "))))

(initials (first data))

(def jds (filter #(= (initials %) "JD") data))

(def jd-ids (set (map :customerid jds)))

(def orders-input (-> (slurp (str "input/hod_2022/noahs-orders.csv"))
                      (str/split #"\r?\n")))


(def order-headers (as-> (first orders-input) input
               (str/split input #",")
               (map keyword input)))

(defn make-order-map
  [line]
  (as-> line line
    (str/split line #",")
    (zipmap order-headers line)))

(def order-data (mapv make-order-map (rest orders-input)))

(filter #(and (str/starts-with? (:ordered %) "2017") (jd-ids (Integer/parseInt (:customerid %)))) order-data)
