(ns advent-of-code.2022.day18
  (:require [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clojure.string :as str]))

(defn count-touch [cubes [x y z]]
  (->> (for [i [-1 1]]
         (cond-> 0
           (contains? cubes [(+ i x) y z]) inc
           (contains? cubes [x (+ i y) z]) inc
           (contains? cubes [x y (+ i z)]) inc))
       (apply +)))

(defn calc-cube [path]
  (->> (str/split (slurp (io/resource path)) #",|\n")
       (map read-string)
       (partition 3)))

(defn part1 [path]
  (->> (calc-cube path)
       (reduce (fn [[acc c] cube]
                 [(conj acc cube) (+ c (- 6 (* 2 (count-touch acc cube))))])
               [#{} 0])
       (second)))

(defn mk-neighbors-fn [minabs maxabs cubes]
  (fn [water [x y z]]
    (->> (for [i [-1 1]] [[(+ x i) y z] [x (+ y i) z] [x y (+ z i)]])
         (apply concat) 
         (filter (partial every? #(<= minabs % maxabs)))
         (filter #(not (or (contains? water %) (contains? cubes %))))
         set)))

(defn part2 [path]
  (let [cubes (set (calc-cube path))
        [minabs maxabs] (apply (juxt (comp dec min) (comp inc max)) (apply concat cubes))
        neighbors (mk-neighbors-fn minabs maxabs cubes)]
    (loop [water #{}
           queue #{(repeat 3 minabs)}
           count 0]
      (let [curr (first queue)
            r    (disj queue curr)]
        (if (nil? curr)
          count
          (recur (conj water curr)
                 (union r (neighbors water curr))
                 (+ count (count-touch cubes curr))))))))

(comment
  (time (part1 "2022/day18"))
  (time (part2 "2022/day18"))
  )