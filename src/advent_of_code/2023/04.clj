(ns advent-of-code.2023.04
  (:require [advent-of-code.utils :as u]
            [clojure.math :refer [pow]]))

(defn read-cards [path]
  (->> (u/read-file-list path u/nums)
       (map #(->> (drop 1 %) (split-at 10)))
       (mapv (fn [[w n]] (count (keep (set w) n))))))

(defn part1 [path]
  (->> (read-cards path)
       (map #(if (= % 0) 0 (pow 2 (dec %))))
       (apply +)))

(defn part2 [path]
 (let [cards (read-cards path)]
   (->> (reduce-kv (fn [r k v]
                     (reduce #(update %1 %2 + (get %1 k)) r (range (inc k) (+ k v 1))))
                   (vec (repeat (count cards) 1)) cards)
        (apply +))))

(comment
  (part1 "2023/4.in") ; ~5.7ms
  (part2 "2023/4.in") ; ~5.9ms
  )