(ns advent-of-code.day1
  (:require [advent-of-code.utils :refer [read-file-list]]))
  

(defn most-calories [resource]
  (->> (read-file-list resource)
       (map (partial reduce +))
       (apply max)))

(defn top3-calories [resource]
  (->> (read-file-list resource)
       (map (partial reduce +))
       (sort >)
       (take 3)
       (apply +)))

(comment
  (most-calories "day1")
  (top3-calories "day1")
  )
