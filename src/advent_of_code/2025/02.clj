(ns advent-of-code.2025.02
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2025 2))

(defn solve [path part2?]
  (let [reg (if part2? #"(\d+)\1+" #"(\d+)\1")]
    (->> (u/read-file-line path #(re-seq #"\d+" %))
         (map parse-long)
         (partition 2)
         (mapcat (fn [[x y]] (for [i (range x (inc y))] (str i))))
         (keep #(some-> (re-matches reg %)
                        first
                        parse-long))
         (reduce +))))

(comment
  (solve path false)
  (solve path true)
  ;;
  )