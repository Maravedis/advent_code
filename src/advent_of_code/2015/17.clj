(ns advent-of-code.2015.17
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as c]))

(defn part1 [path total]
  (let [input   (map-indexed #(vector %1 %2) (u/read-file-list path))
        subsets (c/subsets input)]
    (count (for [sub   subsets
                 :when (= total (apply + (map second sub)))]
             sub))))

(defn part2 [path total]
  (let [input   (map-indexed #(vector %1 %2) (u/read-file-list path))
        subsets (c/subsets input)
        holders (->> (for [sub   subsets
                           :when (= total (apply + (map second sub)))]
                       sub)
                     (group-by count))]
    (count (get holders (apply min (keys holders))))))

(comment
  (def path (u/get-input 2015 17))
  (def tpath "2015/17_test.in")

  (part1 tpath 25)
  (part1 path 150)
  (part2 tpath 25)
  (part2 path 150))