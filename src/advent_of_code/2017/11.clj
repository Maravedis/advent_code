(ns advent-of-code.2017.11
  (:require [advent-of-code.utils :as u]
            [advent-of-code.hexgrid :as h]
            [clojure.string :refer [split]]))

(def path (u/get-input 2017 11))
(def tpath (u/test-path 2017 11))

; We're rotating the direction because my hexgrid is not oriented the same way
(def dir-map {"n"  h/E
              "sw" h/NW
              "nw" h/NE
              "s"  h/W
              "ne" h/SE
              "se" h/SW})

(defn parse-dirs [line]
  (->> (split line #",")
       (map dir-map)))

(defn part1 [path]
  (->> (u/read-file-line path parse-dirs)
       (reduce #(h/move %1 %2) h/origin)
       h/magnitude))

(defn part2 [path]
  (->> (u/read-file-line path parse-dirs)
       (reduce (fn [[pos curr-max] dir]
                 (let [new-pos (h/move pos dir)]
                   [new-pos (max curr-max (h/magnitude new-pos))])) [h/origin 0])
       second))

(comment

  (part1 tpath)
  (part1 path)

  (part2 path))
