(ns advent-of-code.2023.day2
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]))

(defn read-draw [draw]
  (->> (split draw #", ")
       (map #(split % #" "))
       (map (fn [[n color]] [(keyword color) (read-string n)]))
       (into {})))

(defn read-game [line]
  (->> (split line #"(:|;) ")
       (drop 1)
       (map read-draw)
       (reduce #(reduce (fn [res [color n]] (update res color (fnil max n) n)) %1 %2))))

(defn gen-solve [path f]
  (->> (u/read-file-list path read-game)
       (keep-indexed (fn [id {:keys [red blue green]}] (f id red blue green)))
       u/sum))

(comment
  ; part1
  (gen-solve "2023/2.in" (fn [id r b g] (when (and (<= r 12) (<= g 13) (<= b 14)) (inc id))))
  ;part2
  (gen-solve "2023/2.in" (fn [_ r b g] (* r b g)))
  )