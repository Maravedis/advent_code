(ns advent-of-code.2023.01
  (:require [advent-of-code.utils :as u]))

(defn part1 [path]
  (->> (u/read-file-list path #(re-seq #"\d" %))
       (map #(+ (* 10 (parse-long (first %))) (parse-long (last %))))
       (apply +)))

(defn part2 [path]
  (->> (u/read-file-list path #(re-seq #"(?=(one|two|three|four|five|six|seven|eight|nine|\d))" %))
       (map #(+ (* 10 (u/digit-map (second (first %)))) (u/digit-map (second (last %)))))
       (apply +)))

(comment 
  (part1 "2023/1.in") ; ~857µs
  (part2 "2023/1.in") ; ~3.2ms 
  )
  