(ns advent-of-code.day4
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
  (count-pairs "day4" enclosed?)
  (count-pairs "day4" overlap?))