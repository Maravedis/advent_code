(ns advent-of-code.2024.25
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2024 25))
(def tpath (u/test-path 2024 25))

(defn ->heights [item]
  (->> (map u/transpose item)
       (map (fn [col] (map #(dec (u/count-when (fn [i] (= i \#)) %)) col)))))

(defn part1 [path]
  (let [{locks true
         keys  false} (->> (u/read-file-segmented-list path vec)
                           (group-by #(= (ffirst %) \#)))]
    (->> (for [lock  (->heights locks)
               key   (->heights keys)
               :let  [overlap (map + lock key)]
               :when (every? #(<= % 5) overlap)]
           :fit)
         count)))

(comment
  (part1 path))
