(ns advent-of-code.2015.16
  (:require [advent-of-code.utils :as u]
            [clojure.set :as s]))

(def gift
  (set {:children    3
        :cats        7
        :samoyeds    2
        :pomeranians 3
        :akitas      0
        :vizslas     0
        :goldfish    5
        :trees       3
        :cars        2
        :perfumes    1}))

(defn parse-line [line]
  (let [id        (-> (re-find #"Sue (\d+)" line)
                      last
                      parse-long)
        compounds (->> (re-seq #"(\w+): (\d+)" line)
                       (map (fn [[_ kind num]] [(keyword kind) (parse-long num)]))
                       (set))]
    [compounds id]))

(defn check [compounds]
  (let [c1 (into {} compounds)
        c2 (set (dissoc c1 :cats :trees :pomeranians :goldfish))]
    (and (s/subset? c2 gift)
         (> (get c1 :trees 100) 3)
         (> (get c1 :cats 100) 7)
         (< (get c1 :pomeranians 0) 3)
         (< (get c1 :goldfish 0) 5))))

(defn part1 [path]
  (let [sues (->> (u/read-file-list path parse-line)
                  (group-by first)
                  (map (fn [[k v]] [k (map second v)])))]
    (reduce (fn [acc [compounds ids]]
              (if (s/subset? compounds gift)
                (concat acc ids)
                acc)) [] sues)))

(defn part2 [path]
  (let [sues (->> (u/read-file-list path parse-line)
                  (group-by first)
                  (map (fn [[k v]] [k (map second v)])))]
    (reduce (fn [acc [compounds ids]]
              (if (check compounds)
                (concat acc ids)
                acc)) [] sues)))

(comment
  (def path (u/get-input 2015 16))

  (part1 path)
  (part2 path))