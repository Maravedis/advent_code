(ns advent-of-code.2024.03
  (:require [advent-of-code.utils :as u]
            [dom-top.core :refer [loopr]]))

(defn part1 [path]
  (->> (u/read-file-list path #(re-seq #"mul\((\d{1,3}),(\d{1,3})\)" %))
       (apply concat)
       (reduce (fn [acc [_ x y]] (+ acc (* (parse-long x) (parse-long y)))) 0)))

(defn part2 [path]
  (loopr [on true
          acc 0]
         [[h1 h2 h3] (->> (u/read-file-list path #(re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" %))
                          (apply concat))] 
         (if (nil? h2)
           (recur (= h1 "do()") acc)
           (recur on (if on (+ acc (* (parse-long h2) (parse-long h3))) acc)))
         acc))

(comment
  (def path (u/get-input 2024 3))
  (def tpath "2024/3_test.in")
  (part1 path)
  (time (part2 path))
  )