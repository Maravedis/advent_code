(ns advent-of-code.2024.19
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [starts-with?]]))

(def arrangements
  (u/memoize-n
   (fn [curr design patterns]
     (cond (= curr design) 1
           (>= (count curr) (count design)) 0
           (not (starts-with? design curr)) 0
           :else (reduce + (map #(arrangements (str curr %) design patterns) patterns))))
   2))

(defn part1 [path]
  (let [[[patterns] designs] (u/read-file-segmented-list path identity)
        patterns             (re-seq #"\w+" patterns)]
    (u/count-when #(< 0 (arrangements "" % patterns)) designs)))

(defn part2 [path]
  (let [[[patterns] designs] (u/read-file-segmented-list path identity)
        patterns             (re-seq #"\w+" patterns)]
    (reduce #(+ %1 (arrangements "" %2 patterns)) 0 (vec designs))))

(comment
  (def path (u/get-input 2024 19))
  (def tpath (u/test-path 2024 19))

  (part1 path)
  (part2 path))