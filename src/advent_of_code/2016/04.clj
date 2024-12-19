(ns advent-of-code.2016.04
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [join] :as str]))

(def path (u/get-input 2016 04))
(def tpath (u/test-path 2016 04))

(defn parse-line [line]
  (re-matches #"([a-z-]+)-(\d+)\[(\w+)\]" line))

(defn is-real [[_ room _ check]]
  (let [freqs (->> (dissoc (frequencies room) \-)
                   (sort-by second >)
                   (map first)
                   (take 5))]
    (= (set freqs) (set check))))

(defn part1 [path]
  (let [input (->> (u/read-file-list path parse-line))]
    (->> (filter is-real input)
         (reduce (fn [acc [_ _ sId]] (+ acc (parse-long sId))) 0))))

(defn decrypt [[_ room sId]]
  (->> (reduce #(conj %1 (if (= %2 \-) " "
                             (char (+ (mod (+ (parse-long sId) (- (int %2) (int \a))) 26) (int \a)))))
               [] room)
       join
       (vector sId)))

(defn part2 [path]
  (let [input (->> (u/read-file-list path parse-line))]
    (->> (filter is-real input)
         (map decrypt)
         (filter #(str/includes? (second %) "north")))))

(comment
  (part1 path)
  (part2 path))
