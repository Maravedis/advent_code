(ns advent-of-code.2015.24
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as c]
            [clojure.core.reducers :as r]))
; Those won't work for the sample input. The combinations number was found by analyzing the input

(defn part1 [path]
  (let [input  (flatten (u/read-file-list path u/nums))
        target (/ (u/sum input) 3)]
    (->> (c/combinations input 6)
         (r/filter #(= target (u/sum %)))
         (r/map #(apply *' %))
         (r/fold (r/monoid min (constantly Long/MAX_VALUE))))))

(defn part2 [path]
  (let [input  (flatten (u/read-file-list path u/nums))
        target (u/tee (/ (u/sum input) 4))]
    (->> (c/combinations input 4)
         (r/filter #(= target (u/sum %)))
         (r/map #(apply *' %))
         (r/fold (r/monoid min (constantly Long/MAX_VALUE))))))

(comment
  (def path (u/get-input 2015 24))

  (part1 path)
  (part2 path))