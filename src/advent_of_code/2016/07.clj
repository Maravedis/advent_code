(ns advent-of-code.2016.07
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [intersection]]
            [clojure.string :as str]))

(def path (u/get-input 2016 07))
(def tpath (u/test-path 2016 07))

(defn parse-line [line]
  {:inside  (map second (re-seq #"\[(\w+)\]" line))
   :outside (str/split line #"\[\w+\]")})

(defn has-abba? [s]
  (->> (partition 4 1 s)
       (some (fn [[a b c d]] (and (= a d) (= b c) (not= a b))))))

(defn part1 [path]
  (->> (u/read-file-list path parse-line)
       (filter (fn [{:keys [inside outside]}] (and (some has-abba? outside)
                                                   (not-any? has-abba? inside))))
       count))

(defn all-aba [s]
  (->> (partition 3 1 s)
       (filter (fn [[a b c]] (and (= a c) (not= a b))))))

(defn aba->bab [[a b _]] [b a b])

(defn part2 [path]
  (->> (u/read-file-list path parse-line)
       (filter (fn [{:keys [inside outside]}]
                 (let [abas-out (set (mapcat all-aba outside))
                       abas-in  (set (map aba->bab (mapcat all-aba inside)))]
                   (not-empty (intersection abas-out abas-in)))))
       count))

(comment
  (part1 path)
  (part2 path))
