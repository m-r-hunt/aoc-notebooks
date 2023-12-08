(ns aoc-notebooks.utils)


;; GCD/LCM Shamelessly joinked from stackoverflow: https://stackoverflow.com/questions/3078811/why-isnt-this-running-in-constant-space-and-how-do-i-make-it-so-it-does

(defn gcd
  ([x y]
   (cond (zero? x) y
         (< y x)   (recur y x)
         :else     (recur x (rem y x))))
  ([x y & zs]
   (reduce gcd (gcd x y) zs)))

(defn lcm
  ([x y] (/ (* x y) (gcd x y)))
  ([x y & zs]
   (reduce lcm (lcm x y) zs)))

(defn digit?
  [d]
  (re-matches #"[0-9]" (str d)))
