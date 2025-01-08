(ns advent-of-code.2016.20
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(def path (u/get-input 2016 20))

(def upper-limit 4294967296)

(defn parse-range [s]
  (->> (str/split s #"-")
       (mapv parse-long)))

(defn part1 [path]
  (->> (u/read-file-list path parse-range)
       sort
       (reduce (fn [acc [start end]]
                 (if (<= start acc)
                   (inc end)
                   (reduced acc))) 0)))

(defn part2 [path]
  (->> (u/read-file-list path parse-range)
       (cons [upper-limit upper-limit])
       sort
       (reduce (fn [[open-range acc] [start end]]
                 (if (<= start open-range)
                   [(max open-range (inc end)) acc]
                   [(inc end) (+ acc (- start open-range))])) [0 0])
       second))

(comment

  (time (part1 path))
  (time (part2 path)))
