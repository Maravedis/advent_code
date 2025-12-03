(ns advent-of-code.2025.03
  (:require
   [advent-of-code.utils :as u]
   [clojure.string :as str]))

(def path (u/get-input 2025 3))

(defn next-best-fn
  [length n]
  (fn [acc idx x]
    (or (some #(when (and (> x (acc %)) (< idx (+ (- n length) % 1)))
                 (reduce conj (conj (subvec acc 0 %) x) (repeat (- length % 1) 0)))
              (range length))
        acc)))

(defn solve [path length]
  (->> (u/read-file-list path #(mapv (comp parse-long str) %))
       (map #(reduce-kv (next-best-fn length (count %)) (vec (repeat length 0)) %))
       (map (comp parse-long str/join))
       (reduce +)))

(comment
  (solve path 2)
  (solve path 12))
