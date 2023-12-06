(ns advent-of-code.2023.02
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]))

(defn read-game [line]
  (->> (split line #"(:|;|,)? ")
       (drop 2)
       (partition 2)
       (reduce (fn [r [n c]] (update r (keyword c) max (parse-long n))) {:blue 0 :red 0 :green 0})))

(defn gen-solve [path f]
  (->> (u/read-file-list path read-game)
       (keep-indexed (fn [id {:keys [red blue green]}] (f id red blue green)))
       u/sum))

(comment
  ; part1 ~1.5 ms
  (gen-solve "2023/2.in" (fn [id r b g] (when (and (<= r 12) (<= g 13) (<= b 14)) (inc id))))
  ;part2 ~1.5 ms 
  (gen-solve "2023/2.in" (fn [_ r b g] (* r b g)))
  )