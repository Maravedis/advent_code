(ns advent-of-code.2023.11
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]))

(defn empty-cols [input]
  (let [h (count input)]
    (for [col   (range 0 (count (first input)))
          :when (reduce (fn [res row] (if (= \# (get-in input [row col])) (reduced false) res)) true (range 0 h))]
      col)))

(defn parse-galaxies [path offset]
  (let [input (vec (u/read-file-list path vec))
        e-c   (empty-cols input)
        ex-1  (loop [[line & t] input
                     i          0
                     result     []]
                (if line
                  (if-let [galaxies (not-empty (reduce-kv (fn [res idx x] (if (= x \#) (conj res [i idx]) res)) [] line))]
                    (recur t (inc i) (concat result galaxies))
                    (recur t (+ offset i) result))
                  result))]
    (map (fn [[row col]]
           (let [x (count (take-while #(> col %) e-c))]
             [row (+ col (* -1 x) (* offset x))])) ex-1)))

(defn solve [path offset]
  (let [galaxies (parse-galaxies path offset)]
    (loop [[x & t] galaxies
           result  0]
      (if x
        (recur t (+ result (reduce (fn [r y] (+ r (p/manhattan x y))) 0 t)))
        result))))

(comment
  (def path (u/get-input 2023 11))

  (solve path 2)
  (solve path 1000000)
  )