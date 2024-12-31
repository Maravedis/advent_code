(ns advent-of-code.2016.13
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]))

(def path (u/get-input 2016 13))
(def tpath (u/test-path 2016 13))

(defn is-wall? [[x y] fav-num]
  (loop [value (+ (* x x) (* x 3) (* 2 x y) y (* y y) fav-num)
         bits  0]
    (if (= 0 value)
      (odd? bits)
      (recur (quot value 2) (cond-> bits (= 1 (mod value 2)) inc)))))

(defn neighbors-fn [input]
  (fn [curr]
    (p/neighbours curr #(when (and (<= 0 (first %)) (<= 0 (second %))) (not (is-wall? % input))))))

(defn part1 [path end]
  (let [input (u/read-file-line path parse-long)
        start [1 1]]
    (p/a-star-score start
                    end
                    (neighbors-fn input)
                    p/manhattan)))

(defn part2 [path]
  (let [input     (u/read-file-line path parse-long)
        start     [1 1]
        in-radius (p/radius-n 50 [1 1] (fn [[x y]] (and (>= x 0) (>= y 0) (not (is-wall? [x y] input)))))]
    (loop [open      (set in-radius)
           reachable (transient #{})]
      (if-let [curr (first open)]
        (let [way (p/a-star start curr (neighbors-fn input) p/manhattan)]
          (if (= way :blocked)
            (recur (disj open curr) reachable)
            (recur (reduce disj open way) (reduce conj! reachable (take 51 way)))))
        (count reachable)))))

(comment

  (part1 tpath [7 4])
  (part1 path [31 39])

  (part2 path))
