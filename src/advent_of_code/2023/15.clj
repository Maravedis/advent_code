(ns advent-of-code.2023.15
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]
            [clojure.string :refer [split]]))

(defn ascii-hash [in]
  (reduce (fn [res h] (mod (* (+ res (int h)) 17) 256)) 0 in))

(defn update-box [content label len]
  (let [idx (first (keep-indexed (fn [idx [la]] (when (= la label) idx)) content))]
    (cond (and idx len) (assoc content idx [label (parse-long len)])
          idx           (vec (concat (take idx content) (drop (inc idx) content)))
          len           (conj content [label (parse-long len)])
          :else         content)))

(defn parse [line]
  (let [ins (re-seq #"(\w+)(-|=(\d+))" line)]
    (reduce (fn [res [_ box _ len]] (update res (ascii-hash box) update-box box len))
            (vec (repeat 256 [])) ins)))

(defn part1 [path]
  (let [input (first (u/read-file-list path (comp vec #(split % #","))))]
    (r/fold (int (/ (count input) 8)) + #(+ %1 (ascii-hash %2)) input)))

(defn part2 [path]
  (let [boxed (first (u/read-file-list path parse))]
    (reduce-kv (fn [res b-idx content]
                 (reduce-kv (fn [r c-idx [_ len]]
                              (+ r (* (inc b-idx) (inc c-idx) len))) res content)) 0 boxed)))

(comment 
  (def path (u/get-input 2023 15))
  (part1 path)
  (part2 path)
  )