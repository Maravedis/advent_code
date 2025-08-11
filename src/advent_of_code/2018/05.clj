(ns advent-of-code.2018.05
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2018 5))
(def tpath (u/test-path 2018 5))

(defn part1 [path]
  (->> (u/read-file-line path)
       (map int)
       (reduce (fn [acc letter]
                 (cond (empty? acc) [letter]
                       (= 32 (abs (- (peek acc) letter))) (pop acc)
                       :else (conj acc letter))) [])
       count))

(defn part2 [path]
  (let [polymer (map int (u/read-file-line path))]
    (reduce (fn [shortest unit]
              (->> polymer
                   (reduce (fn [acc letter]
                             (cond (or (= unit letter) (= unit (- letter 32))) acc
                                   (empty? acc) [letter]
                                   (= 32 (abs (- (peek acc) letter))) (pop acc)
                                   :else (conj acc letter))) [])
                   count
                   (min shortest))) Integer/MAX_VALUE (range (int \A) (+ (int \A) 26)))))

(comment
  (part1 tpath)
  (part1 path)

  (part2 tpath)
  (part2 path))
