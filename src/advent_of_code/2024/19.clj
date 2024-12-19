(ns advent-of-code.2024.19
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [starts-with?]]))

(def arrangements
  (memoize
   (fn [patterns design curr]
     (cond (= curr design) 1
           (>= (count curr) (count design)) 0
           (not (starts-with? design curr)) 0
           :else (reduce + (map #(arrangements patterns design (str curr %)) patterns))))))

(defn part1 [path]
  (let [[[patterns] designs] (u/read-file-segmented-list path identity)
        patterns             (re-seq #"\w+" patterns)]
    (u/count-when #(< 0 (arrangements patterns % "")) designs)))

(defn part2 [path]
  (let [[[patterns] designs] (u/read-file-segmented-list path identity)
        patterns             (re-seq #"\w+" patterns)]
    (reduce #(+ %1 (arrangements patterns %2 "")) 0 (vec designs))))

(comment
  (def path (u/get-input 2024 19))
  (def tpath (u/test-path 2024 19))

  (part1 path)
  (part2 path))