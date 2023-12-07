(ns advent-of-code.2015.08
  (:require [advent-of-code.utils :as u]))

(defn part1 [path]
  (->> (u/read-file-list path #(- (count %) (count (re-seq #"(\w)|(\\x[0-9a-f]{2})|(\\\")|(\\\\)" %))))
       (apply +)))

(defn part2 [path]
  (->> (u/read-file-list path #(- (->> (re-seq #"(\w|\d)|(\"|\\)" %)
                                       (reduce (fn [r [_ a b]] (+ r (cond a 1 b 2))) 2))
                                  (count %)))
       (apply +)))

(comment
  (def path (u/get-input 2015 8))
  (part1 path)
  (part2 path)
  )