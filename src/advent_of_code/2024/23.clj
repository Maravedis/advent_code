(ns advent-of-code.2024.23
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [intersection]]
            [clojure.string :refer [join]]))

(def path (u/get-input 2024 23))
(def tpath (u/test-path 2024 23))

(defn parse-network [path]
  (->> (u/read-file-list path #(re-seq #"\w+" %))
       (reduce (fn [acc [c1 c2]] (-> (update acc c1 (fnil conj #{})  c2)
                                     (update c2 (fnil conj #{})  c1))) {})))

(defn ->sub-network [agg-fn network]
  (->> (reduce-kv (fn [acc c1 connections]
                    (reduce (fn [acc c2]
                              (let [v (intersection connections (network c2))]
                                (if (<= 1 (count v))
                                  (agg-fn acc v c1 c2)
                                  acc))) acc connections)) [] network)
       distinct))

(defn part1 [path]
  (->> (parse-network path)
       (->sub-network (fn [acc v c1 c2] (apply conj acc (map #(set [% c1 c2]) v))))
       (filter (fn [c] (some #(= \t (.charAt % 0)) c)))
       count))

(defn part2 [path]
  (let [network (parse-network path)]
    (->> (->sub-network #(conj %1 (conj %2 %3 %4)) network)
         (filter #(every? (fn [c] (every? (fn [x] ((network x) c)) (disj % c))) %))
         (sort-by count >)
         first
         sort
         (join ","))))

(comment
  (part1 path)
  (part2 path))
