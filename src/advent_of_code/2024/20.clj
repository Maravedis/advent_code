(ns advent-of-code.2024.20
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2024 20))
(def tpath (u/test-path 2024 20))

(defn radius-n [n]
  (fn [grid [x y :as start]]
    (for [r     (range (* -1 n) (inc n))
          c     (range (* -1 n) (inc n))
          :let  [res [(+ r x) (+ c y)]]
          :when (and (not= 0 r c)
                     (grid res)
                     (<= (p/manhattan start res) n))]
      res)))

(defn solve [path jump-fn]
  (let [input       (vec (u/read-file-list path vec))
        grid        (p/->grid input)
        start       (ffirst (filter #(= \S (second %)) grid))
        end         (ffirst (filter #(= \E (second %)) grid))
        way         (-> (p/a-star grid start end)
                        (p/traceback start end))
        way-indexed (->> (map-indexed #(vector %2 %1) way)
                         (into {}))]
    (->> (r/mapcat (fn [s c]
                     (->> (jump-fn way-indexed s)
                          (r/map #(when-let [n (way-indexed %)] (and (> n c) (- n c (p/manhattan % s)))))
                          (r/filter identity))) way-indexed)
         (r/filter #(>= % 100))
         (r/fold + (fn [acc _] (inc acc))))))

(comment
  (time (solve path (radius-n 2)))
  (time (solve path (radius-n 20))))

