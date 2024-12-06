(ns advent-of-code.points
  (:require [clojure.core.reducers]))

(def origin [0 0])
(def N [-1 0])
(def E [0 1])
(def S [1 0])
(def W [0 -1])
(def cardinals [N E S W])

(def NE [-1 1])
(def SE [1 1])
(def NW [-1 -1])
(def SW [1 -1])

(defn ->grid
  "takes a 2D array and return a sorted map of {[x y] value}"
  [input]
  (->> (vec input)
       (reduce-kv
        (fn [acc r row]
          (reduce-kv (fn [acc c v] (assoc! acc [r c] v))
                     acc (vec row)))
        (transient {}))
       persistent!))

(defn move
  "Given a starting point and a direction, move in that direction on a grid. If n is supplied, repeat n times."
  ([point direction] (mapv + point direction))
  ([point direction n] (case n
                         0 point
                         1 (mapv + point direction)
                         (mapv + point (map #(* n %) direction)))))

(defn extract-line
  "Given a 2d vector grid, extract a line of length n, from orig into direction.
   Truncate the line if it encounters an edge"
  [grid orig direction length]
  (let [height (count grid)
        width  (count (first grid))
        coords (->> (for [l (range length)] (move orig direction l))
                    (filter (fn [[r c]] (and (< -1 r height) (< -1 c width)))))]
    (map #(get-in grid %) coords)))

(defn manhattan
  "Manhattan distance between two points in the [x y] format"
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn shoelace
  "Given a list of vertices in the [x y] format representing a polygon, calculate the area of the polygon.
   The list must be of contiguous points. The first and last vertices must be equal for a loop to be formed."
  [points]
  (/ (abs (reduce (fn [res [[a b] [c d]]] (+ res (- (* a d) (* b c)))) 0 (partition 2 1 points))) 2))

(defn area-vertices
  "Given a list of vertices in the [x y] format representing a polygon, calculate the number of vertices in the polygon (edge included).
    The list must be of contiguous points. The first and last vertices must be equal for a loop to be formed."
  [points]
  (+ 1
     (shoelace points)
     (/ (reduce #(+ %1 (apply manhattan %2)) 0 (partition 2 1 points)) 2)))


