(ns advent-of-code.2016.02
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]))

(def square (p/->grid [[1 2 3]
                       [4 5 6]
                       [7 8 9]]))

(def octo (->> [[0  0  1]
                [0  2  3  4]
                [5  6  7  8  9]
                [0 \A \B \C]
                [0  0 \D]]
               (p/->grid)
               (remove #(= (second %) 0))
               (into {})))

(def ->dir {\U p/N
            \L p/W
            \R p/E
            \D p/S})

(defn solve [path grid start]
  (let [input (->> (u/read-file-list path vec))]
    (->> (reductions
          (fn [curr row]
            (reduce (fn [curr dir]
                      (let [next (p/move curr (->dir dir))]
                        (if (grid next) next curr))) curr row)) start input)
         (drop 1)
         (map grid))))

(comment
  (def path (u/get-input 2016 2))
  (def tpath (u/test-path 2016 2))

  (solve path square [1 1])
  (solve path octo [2 0]))