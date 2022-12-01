(ns advent-of-code.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn most-calories [resource]
  (let [list (slurp (io/resource resource))]
    (->> (str/split list #"\n\n")
         (map str/split-lines)
         (map #(map read-string %))
         (map (partial reduce +))
         (apply max))))

(defn top3-calories [resource]
  (let [list (slurp (io/resource resource))]
    (->> (str/split list #"\n\n")
         (map str/split-lines)
         (map #(map read-string %))
         (map (partial reduce +))
         (sort)
         (reverse)
         (take 3)
         (reduce +))))



(top3-calories "day1_input")