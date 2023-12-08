(ns advent-of-code.2023.08
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split ends-with?]]
            [clojure.math.numeric-tower :refer [lcm]]))

(defn parse-node [line]
  (let [[a b c] (split line #" = \(|, |\)")] 
    [a [b c]]))

(defn calc-path [ins graph start]
  (loop [curr     start
         [in & t] (map #(case % \L 0 \R 1) (cycle ins))
         i        0]
    (if (ends-with? curr "Z")
      i
      (recur (get-in graph [curr in]) t (inc i)))))

(defn part1 [path]
  (let [[[ins] nodes] (u/read-file-segmented-list path identity)
        graph         (->> (map parse-node nodes)
                           (into {}))]
    (calc-path ins graph "AAA")))


(defn part2 [path]
  (let [[[ins] nodes] (u/read-file-segmented-list path identity)
        graph         (->> (map parse-node nodes)
                           (into {}))
        starts        (filter #(ends-with? % "A") (keys graph))]
    (->> (for [start starts] (calc-path ins graph start))
         (reduce lcm))))

(comment
  (def path (u/get-input 2023 8))
  (time (part1 path))
  (time (part2 path))
  )