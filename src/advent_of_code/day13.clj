(ns advent-of-code.day13
  (:require [advent-of-code.utils :as u]
            [clojure.edn :as edn]))

(defn compare-packets
  [left right]
  (if (and (int? left) (int? right))
    (Long/compare left right)
    (let [left  (if (vector? left) left [left])
          right (if (vector? right) right [right])]
      (or (first (remove #(= 0 %) (mapv compare-packets left right)))
          (Long/compare (count left) (count right))))))

(defn count-ordered [path]
  (->> (u/read-file-segmented-list path edn/read-string)
       (map #(apply compare-packets %))
       (keep-indexed (fn [idx item] (when ( < item 0) (inc idx))))
       (apply +)))

(defn find-distress [path]
  (->> (u/read-file-segmented-list path edn/read-string)
       (reduce concat)
       (cons [[2]])
       (cons [[6]])
       (sort compare-packets)
       (keep-indexed (fn [idx item] (when (or (= item [[2]]) (= item [[6]])) (inc idx))))
       (apply *)))



(comment
  (count-ordered "day13")
  (find-distress "day13")
  )