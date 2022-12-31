(ns advent-of-code.2022.day4
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn read-sections [line]
  (->> (str/split line #",")
       (map #(str/split % #"-"))
       (map #(map read-string %))))

(defn enclosed? [[lf ls] [rf rs]]
  (or (and (<= lf rf) (>= ls rs))
      (and (>= lf rf) (<= ls rs))))

(defn overlap? [[lf ls] [rf rs]]
  (if (<= lf rf) (>= ls rf) (<= lf rs)))

(defn count-pairs [resource f]
  (->> (u/read-file-list resource read-sections)
       (filter #(apply f %))
       count))

(comment
  (count-pairs "2022/day4" enclosed?)
  (count-pairs "2022/day4" overlap?))