(ns advent-of-code.day15
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn manhattan [[x y] [x' y']]
  (+ (abs (- x x')) (abs (- y y'))))

(defn read-sensor-beacon [line]
  (let [input   (str/split line #"([,:]? )|=")
        coor-fn (fn [i] (read-string (first (drop i input))))
        sx      (coor-fn 3)
        sy      (coor-fn 5)
        bx      (coor-fn 11)
        by      (coor-fn 13)]
    [[sx sy] (manhattan [sx sy] [bx by])]))

(defn row-intersections [y [[a b] r]]
  (let [delta (- r (abs (- y b)))]
    (if (< delta 0)
      nil
      [(+ a (* -1 delta)) (+ a delta)])))

(defn merge-overlap [[h & r]]
  (reduce (fn [acc [x3 x4]]
            (let [[x1 x2] (peek acc)]
              (cond (and (<= (- x3 x2) 1) (< x2 x4)) (conj (pop acc) [x1 x4])
                    (>= x2 x3) acc
                    :else (conj acc [x3 x4]))))
          [h]
          r))

(defn calc-segments [row input]
  (->> (map (partial row-intersections row) input)
       (filter some?)
       sort
       merge-overlap))

(defn part1 [path row]
  (->> (u/read-file-list path read-sensor-beacon)
       (calc-segments row)
       (map (fn [[x y]] (abs (- y x))))
       (apply +)))

(defn calc-frequency [y [[_ x]]]
  (+ y (* (inc x) 4000000)))

(defn part2 [path rows]
  (let [input (u/read-file-list path read-sensor-beacon)]
    (->> (for [y (range (inc rows))]
           (->> input
                (calc-segments y)))
         (map-indexed vector)
         (drop-while (fn [[_ [[_ x] & r]]]
                       (not (and (some? r)
                                 (<= 0 x rows)))))
         first
         (apply calc-frequency))))

(comment
  (time (part1 "day15" 2000000))
  (time (part2 "day15" 4000000)) 
  )