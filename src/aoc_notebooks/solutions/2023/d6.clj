;; # 2023 Day 6

^{:nextjournal.clerk/visibility {:code :hide}}
(ns aoc-notebooks.solutions.2023.d6
  (:require [aoc-notebooks.input :as input]
            [clojure.math :as math]))

;; [Problem.](https://adventofcode.com/2023/day/6) Summary:

;; > As part of signing up, you get a sheet of paper (your puzzle input) that lists the time allowed for each race and also the best distance ever recorded in that race. To guarantee you win the grand prize, you need to make sure you go farther in each race than the current record holder.

;; > The organizer brings you over to the area where the boat races are held. The boats are much smaller than you expected - they're actually toy boats, each with a big button on top. Holding down the button charges the boat, and releasing the button allows the boat to move. Boats move faster if their button was held longer, but time spent holding the button counts against the total race time. You can only hold the button at the start of the race, and boats don't move until the button is released.

;; > Your toy boat has a starting speed of zero millimeters per millisecond. For each whole millisecond you spend at the beginning of the race holding down the button, the boat's speed increases by one millimeter per millisecond.

;; > Determine the number of ways you could beat the record in each race. What do you get if you multiply these numbers together?

;; ## Part 1

;; Looking at the problem, two possible paths occured to me.
;; One is to do a simple brute force loop: for each race, try every possible charge time and count up the ones that meet the distance requirement.
;; The other is to apply some maths and directly calculate the range of winning values.
;; I settled on the mathsy approach because I had time to think about how to do it, and it seemed like it would turn out quite elegant.
;; This turned out to be a great advantage for part 2 as we'll see later.

;; Let the length of the race in ms be $n$. Let the distance record to beat be $d$.

;; If we charge our boat for $k\,\mathrm{ms}$ then our boat gets to travel for the remaining $(n - k)\,\mathrm{ms}$ at a speed of $k\,\mathrm{mm}/\mathrm{ms}$.
;; That gives the total distance at the end of the race as $k(n - k)\,\mathrm{mm}$.

;; This is a quadratic equation in $k$, with roots at $k = 0$ and $k = n$ and a maximum in the middle.
;; We're trying to beat a distance of $d$, which will happen as long as $d$ is less than that maximum.

;; To get the range of $k$ where we beat $d$ we can solve the equation $k(n - k) = d$ to get the end points of the interval, as we know there will be 2 roots and we'll be beating the distance record in between them.
;; Rearranging this equation into a conventional form gives this:

;; $$ k^2 - kn + d = 0 $$

;; Handwaving argument on looking at the graph, but it's not rocket science.
;; Also no error handling for if the equation is unsolvable, assume it won't be for the question to work.

;; That works on the reals (well, floats, you know what I mean). 
;; Since we're only interested in integers for the question we can round off back to discrete values.
;; Round the lower root up and the upper root down to ensure the rounded value still wins.
;; If the solution is already an exact integer we still "round" up/down by 1 because we need to actually beat the record, not just match it.

;; Call the rounded versions of the higher and lower roots $k_r$ and $k_l$ respectively.
;; Then the final count of winning charge times is $k_r - k_l + 1$ (avoiding an off-by-one).

;; Implementing that in code is straightforward using the quadratic formula. `time` is $n$ and `dist` is $d$.

(defn solve
  [time dist]
  (let [det (math/sqrt (- (* time time) (* 4 dist)))
        kl (/ (- time det) 2)
        kr (/ (+ time det) 2)
        rkl (if (<= (math/round kl) kl) (inc (math/round kl)) (math/round kl))
        rkr (if (>= (math/round kr) kr) (dec (math/round kr)) (math/round kr))]
    (inc (- rkr rkl))))

;; We can verify our solution is working as expected using the example cases from the problem:

(solve 7 9) ; Expecting 4
(solve 15 40) ; Expecting 8
(solve 30 200) ; Expecting 9

;; Now we need actually solve the problem for our input. First we suck in the data using my handy input library:

(def input (input/numbers-and-tokens 2023 6))

;; Then we can pull out times and distances (chopping off the labels at the start which we don't need)

(def times (rest (first input)))
(def distances (rest (second input)))

;; And finally we just apply our solution function to all the data and multiply up the results

(reduce * (map solve times distances))

;; ## Part 2

;; Part 2 is really just part 1 with a race running for really big numbers.
;; If we had taken the brute force approach for part 1 we might have run into some problems trying to run through every case.
;; However our maths applies just the same without any modification.

;; We might worry about overflow or precision issues, but Clojure's automatic promotion takes care of any possible overflow, and precision was Good Enough(tm).

;; A quick check of the example input shows we're working fine:

(solve 71530 940200) ; Expecting 71503

;; Then we can reinterpret our input by smushing the numbers together as strings and reparsing them:

(def p2-time (Long/parseLong (apply str (map str times))))

(def p2-dist (Long/parseLong (apply str (map str distances))))

;; And finally we just call the solver to finish

(solve p2-time p2-dist)
