(ns advent-of-code.2016.24
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [advent-of-code.vectors :refer [evict-at]]))

(def path (u/get-input 2016 24))
(def tpath (u/test-path 2016 24))

; Incredibly slow, but I'm lazy today, and it works, so why bother ¯\_(ツ)_/¯
(defn run [path part2?]
  (let [grid     (->> (u/read-file-list path identity)
                      p/->grid)
        start    (ffirst (filter #(#{\0} (second %)) grid))
        targets  (->> (filter #(#{\1 \2 \3 \4 \5 \6 \7 \8 \9} (second %)) grid)
                      (mapv first))
        min-path (fn bfs [curr open]
                   (if (empty? open)
                     (if part2? (p/a-star-score grid curr start) 0)
                     (apply min (map (fn [i] (+ (p/a-star-score grid curr (open i))
                                                (bfs (open i) (evict-at open i)))) (range (count open))))))]
    (time (min-path start targets))))

(comment

  (run path false)
  (run path true)
  ;
  )
