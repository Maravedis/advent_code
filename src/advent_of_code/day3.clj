(ns advent-of-code.day3
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [intersection]]))

(defn ->score [c]
  (cond (<= 97 c 122) (- c 96)
        (<= 65 c 90) (- c 38)))

(defn ->priority [items]
  (->> items
       (map int)
       (map ->score)
       set))

(defn ->compartments [rucksack]
  (let [l (count rucksack)]
    (->> (split-at (/ l 2) rucksack)
         (map ->priority))))

(defn priority [resource]
  (->> (u/read-file-list resource ->compartments)
       (mapcat #(apply intersection %))
       u/sum))

(defn badge [resource]
  (->> (u/read-file-list resource ->priority)
       (partition 3) 
       (mapcat #(apply intersection %))
       u/sum))

(comment
  (priority "day3")
  (badge "day3")
  )
