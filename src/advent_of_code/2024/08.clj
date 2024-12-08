(ns advent-of-code.2024.08
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]
            [clojure.math.combinatorics :as c]
            [criterium.core :refer [quick-bench]]))

(def path (u/get-input 2024 8))
(def input (u/read-file-list path vec))
(def grid (p/->grid input))

(defn antinodes [_ [a b] [c d]]
  [[(- (* 2 a) c) (- (* 2 b) d)]
   [(- (* 2 c) a) (- (* 2 d) b)]])

(defn line [h [x1 y1] [x2 y2]]
  (let [a (/ (- y2 y1) (- x2 x1))
        b (- y1 (* a x1))]
    (for [x     (range 0 h)
          :let  [y (+ (* a x) b)]
          :when (not (ratio? y))]
      [x (int y)])))

(defn solve [antinode-f]
  (->> (reduce-kv #(cond-> %1 (not= %3 \.) (update %3 conj %2)) {} grid)
       (r/mapcat #(c/combinations %2 2))
       (r/mapcat #(apply antinode-f (count input) %))
       (r/filter #(grid %))
       r/foldcat
       set
       count))

(comment

  (quick-bench (solve antinodes)) ; ~244 µs
  ; 247
  (quick-bench (solve line)) ; ~2.5 ms
  ; 861
  )