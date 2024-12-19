(ns advent-of-code.2024.05
  (:require [advent-of-code.utils :as u]
            [com.rpl.specter :as sp]))

(defn is-correct-fn [orders]
  (fn [[h & t]]
    (or (nil? h)
        (and (not-any? #((get orders % #{}) h) t)
             (recur t)))))

(defn sorting-fn [orders]
  (let [ordering (fn [l r]
                   (cond ((get orders r #{}) l) -1
                         ((get orders l #{}) r) 1
                         :else 0))]
    (fn [coll] (sort ordering coll))))

(defn solve [path check-fn list-fn]
  (let [[orders updates] (u/read-file-segmented-list path u/nums)
        orders           (->> orders
                              (group-by first)
                              (sp/transform [sp/MAP-VALS] #(set (map  second %))))
        check            (check-fn orders)
        flist            (list-fn orders)]
    (reduce (fn [acc h]
              (if (check h)
                (+ acc (nth (flist h) (quot (count h) 2)))
                acc)) 0 updates)))

(comment
  (def path (u/get-input 2024 5))
  (def tpath (u/test-path 2024 5))

  (time (solve path is-correct-fn (constantly identity))) ; part1
  (time (solve path (comp complement is-correct-fn) sorting-fn)) ; part2
  )