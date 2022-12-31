(ns advent-of-code.2022.day17
  (:require [clojure.java.io :as io]
            [clojure.set :as s]))

(defn next-shape [highest-point i]
  (let [low-row (+ 4 highest-point)]
    (case (mod i 5)
      0 (map (fn [col] [low-row (+ 2 col)]) (range 4))
      1 (conj (map (fn [col] [(inc low-row) (+ 2 col)]) (range 3)) [low-row 3] [(+ 2 low-row) 3])
      2 (conj (map (fn [col] [low-row (+ 2 col)]) (range 3)) [(inc low-row) 4] [(+ 2 low-row) 4])
      3 (map (fn [row] [row 2]) (range low-row (+ 4 low-row)))
      4 [[low-row 2] [low-row 3] [(inc low-row) 2] [(inc low-row) 3]])))

(defn move [shape dir]
  (case dir
    \< (if (some #(= 0 (second %)) shape) shape (map (fn [[row col]] [row (dec col)]) shape))
    \> (if (some #(= 6 (second %)) shape) shape (map (fn [[row col]] [row (inc col)]) shape))
    \v (map (fn [[row col]] [(dec row) col]) shape)))

(defn fall [path limit]
  (let [jets (slurp (io/resource path))
        len  (count jets)]
    (loop [rocks (set (map (fn [col] [0 col]) (range 7)))
           rock  (next-shape 0 0)
           turn  0
           i     0
           h     0
           added 0
           seen  {}]
      (if (>= turn limit)
        (+ h added)
        (let [moved   (move rock (get jets i))
              moved-s (if (some #(contains? rocks %) moved) rock moved)
              moved-d (move moved-s \v)
              moved   (if (some #(contains? rocks %) moved-d) moved-s moved-d)]
          (if (= moved moved-s)
            (let [new-rocks (s/union rocks moved)
                  new-h     (apply max h (map first moved))
                  memkey    [i (mod turn 5) (set (->> new-rocks
                                                      (filter (fn [[row _]] (<= (- new-h row) 12))) ; honestly do not know what is best here.
                                                      (map (fn [[row col]] [(- new-h row) col]))))]]
              (if-let [[old-turn old-h] (get seen memkey)]
                (let [drow     (- new-h old-h)
                      dturn    (- turn old-turn)
                      period   (quot (- limit turn) dturn)
                      new-turn (inc (+ turn (* period dturn)))]
                  (recur new-rocks (next-shape new-h new-turn) new-turn (mod (inc i) len) new-h (+ added (* period drow)) (assoc seen memkey [turn new-h])))
                (recur new-rocks (next-shape new-h (inc turn)) (inc turn) (mod (inc i) len) new-h added (assoc seen memkey [turn new-h]))))
            (recur rocks moved turn (mod (inc i) len) h added seen)))))))

(comment
  (def part2 1000000000000)
  (fall "2022/day17" 2022)
  (time (fall "2022/day17" part2))
  )