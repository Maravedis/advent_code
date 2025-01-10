(ns advent-of-code.2017.02
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 2))
(def tpath (u/test-path 2017 2))

(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       (reduce (fn [acc row] (+ acc (- (apply max row) (apply min row)))) 0)))

(defn factor [row]
  (loop [[h & t :as ht] row
         [f & r]        (drop 1 row)]
    (let [a (max h f)
          b (min h f)]
      (if (= 0 (mod a b))
        (/ a b)
        (if (empty? r)
          (recur t (drop 1 t))
          (recur ht r))))))

(defn part2 [path]
  (->> (u/read-file-list path u/nums)
       (reduce (fn [acc row] (+ acc (factor row))) 0)))

(comment

  (part1 path)
  (part2 path))
