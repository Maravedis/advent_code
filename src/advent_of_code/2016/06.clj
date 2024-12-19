(ns advent-of-code.2016.06
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]))

(def path (u/get-input 2016 06))
(def tpath (u/test-path 2016 06))

(defn solve [path comp]
  (->> (u/read-file-list path identity)
       (u/transpose)
       (map frequencies)
       (reduce #(str %1 (ffirst (sort-by second comp %2))) "")))

(comment
  (solve path >)
  (solve path <))
