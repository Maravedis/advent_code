(ns advent-of-code.2022.day22
  (:require [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

; PART 1

(defn add-last [x coll]
  (concat coll [x]))

(defn read-maze [path]
  (let [[input cmd] (-> (slurp (io/resource path))
                        (str/split #"\n\n"))
        maze        (->> input
                         str/split-lines
                         (mapv #(->> %
                                     (map (fn [c] (condp = c \space 0 \. 1 \# 2 c)))
                                     (add-last 0)
                                     vec)))
        key (->> (re-seq #"\d+|[A-Z]+" cmd)
                 (map #(condp = % "R" 1 "L" -1 (read-string %))))]
    [key maze]))

(defn move-flat [x steps dir line]
  (let [pad (count (take-while #(= 0 %) line))
        l   (vec (remove #(= 0 %) line))
        len (count l)]
    (loop [i     (- x pad)
           count steps]
      (if (or (< count 0) (= 2 (get l (mod i len))))
        (+ pad (mod (- i dir) len))
        (recur (+ dir i) (dec count))))))

(defn part1 [path]
  (let [[key maze] (read-maze path)]
    (loop [[h rot & r] key
           dir           0
           row           0
           col           (first (keep-indexed #(when (= %2 1) %1) (first maze)))]
      (if (nil? h)
        (+ (* 1000 (inc row)) (* (inc col) 4) dir)
        (if (= 0 (mod dir 2))
          (recur r (mod (+ dir (or rot 0)) 4) row (move-flat col h (* -1 (- dir 1)) (get maze row)))
          (recur r (mod (+ dir (or rot 0)) 4) (move-flat row h (* -1 (- dir 2)) (mapv #(get % col 0) maze)) col))))))

; PART 2

; . 1 2       . A B
; . 3 .       . C .
; 4 5 .       D E .
; 6 . .       F . .

; right 0
; down 1
; left 2
; up 3


(defn ->region [row col]
  (cond
    (and (<= 0 row 49) (<= 50 col 99)) 1
    (and (<= 0 row 49) (<= 100 col 149)) 2
    (and (<= 50 row 99) (<= 50 col 99)) 3
    (and (<= 100 row 149) (<= 0 col 49)) 4
    (and (<= 100 row 149) (<= 50 col 99)) 5
    (and (<= 150 row 199) (<= 0 col 49)) 6
    :else (throw (ex-info "bad coordinates" {:row row
                                             :col col}))))

(defn regionalize [row col region]
  (case region
    1 [row (- col 50)]
    2 [row (- col 100)]
    3 [(- row 50) (- col 50)]
    4 [(- row 100) col]
    5 [(- row 100) (- col 50)]
    6 [(- row 150) col]))

(defn unregionalize [row col region]
  (case region
    1 [row (+ col 50)]
    2 [row (+ col 100)]
    3 [(+ row 50) (+ col 50)]
    4 [(+ row 100) col]
    5 [(+ row 100) (+ col 50)]
    6 [(+ row 150) col]))

; [x y] [a b]. x is the previous region, y is previous direction. a is new region, b is new direction
(def edges {[1 0] [2 0] [1 1] [3 1] [1 2] [4 0] [1 3] [6 0]
            [2 0] [5 2] [2 1] [3 2] [2 2] [1 2] [2 3] [6 3]
            [3 0] [2 3] [3 1] [5 1] [3 2] [4 1] [3 3] [1 3]
            [4 0] [5 0] [4 1] [6 1] [4 2] [1 0] [4 3] [3 0]
            [5 0] [2 2] [5 1] [6 2] [5 2] [4 2] [5 3] [3 3]
            [6 0] [5 3] [6 1] [2 1] [6 2] [1 1] [6 3] [4 3]})

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])

(defn next-point [row-orig col-orig dir]
  (let [region    (->region row-orig col-orig)
        [row col] (regionalize row-orig col-orig region)
        [x y]     (get dirs dir)
        [row col] [(+ row x) (+ col y)]] 
    (if (or (< row 0) (< col 0) (> row 49) (> col 49))
      (let [[new-region new-dir] (edges [region dir])
            [row col]            (match [dir new-dir]
                                   [0 0] [row 0]
                                   [0 2] [(- 49 row) 49]
                                   [0 3] [49 row]

                                   [1 1] [0 col]
                                   [1 2] [col 49]

                                   [2 0] [(- 49 row) 0]
                                   [2 1] [0 row]
                                   [2 2] [row 49]

                                   [3 0] [col 0]
                                   [3 3] [49 col]

                                   :else (throw (ex-info "Too bad" {:dir     dir
                                                                    :new-dir new-dir
                                                                    :row     row
                                                                    :col     col})))
            [row col]            (unregionalize row col new-region)]
        [row col new-dir])
      (let [[row col] (unregionalize row col region)]
        [row col dir]))))

(defn move-cube [row-o col-o steps dir maze]
  (loop [count steps
         row   row-o
         col   col-o
         dir   dir]
    (if (zero? count)
      [row col dir]
      (let [[n-r n-c n-dir] (next-point row col dir)] 
        (if (= 2 (get-in maze [n-r n-c]))
          [row col dir]
          (recur (dec count) n-r n-c n-dir)))))
  )

(defn part2 [path]
  (let [[key maze] (read-maze path)]
    (loop [[steps rot & r] key
           dir             0
           row             0
           col             (first (keep-indexed #(when (= %2 1) %1) (first maze)))]
      (if (nil? steps)
        (+ (* 1000 (inc row)) (* (inc col) 4) dir)
        (let [[row col dir] (move-cube row col steps dir maze)]
          (recur r (mod (+ dir (or rot 0)) 4) row col))))))


(comment

  (time (part1 "2022/day22"))
  (time (part2 "2022/day22")))