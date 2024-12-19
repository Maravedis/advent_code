(ns advent-of-code.2016.03
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2016 03))

(defn part1 [path]
  (let [input (->> (u/read-file-list path u/nums))]
    (u/count-when (fn [[a b c]] (and (> (+ a b) c)
                                     (> (+ a c) b)
                                     (> (+ b c) a))) input)))

(defn part2 [path]
  (let [input   (->> (u/read-file-list path u/nums))
        [a b c] ((juxt #(map first %) #(map second %) #(map (comp first next next) %)) input)]
    (u/count-when (fn [[a b c]] (and (> (+ a b) c)
                                     (> (+ a c) b)
                                     (> (+ b c) a))) (partition 3 (concat a b c)))))

(comment
  (part1 path)
  (part2 path))
