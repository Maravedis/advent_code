(ns advent-of-code.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.math.numeric-tower :refer [lcm]]))

(defn read-blizzards [path]
  (->> (slurp (io/resource path))
       (str/split-lines)
       (drop 1)
       (drop-last)
       (mapv (fn [line] (mapv #(case % \. [] \> [1] \v [2] \< [3] \^ [4]) 
                            (subs line 1 (dec (count line))))))))

(defn update-blizzards [blizzards]
  (let [h (count blizzards)
        w (count (first blizzards))
        canvas (vec (repeat h (vec (repeat w []))))]
    (reduce-kv
     (fn [acc row line]
       (reduce-kv
        (fn [acc1 col chrs]
          (reduce (fn [acc2 chr]
                    (case chr
                      1 (update-in acc2 [row (mod (inc col) w)] #(conj % 1))
                      2 (update-in acc2 [(mod (inc row) h) col] #(conj % 2))
                      3 (update-in acc2 [row (mod (dec col) w)] #(conj % 3))
                      4 (update-in acc2 [(mod (dec row) h) col] #(conj % 4))))
                  acc1 chrs)) 
        acc line))
     canvas blizzards)))

(defn neighbors [[row col] h w canvas] 
  (->> [[row col] [row (inc col)] [(inc row) col] [(dec row) col] [row (dec col)]]
       (remove (fn [[r c]] (and (and (not= [r c] [-1 0]) (not= [r c] [h (dec w)]))
                                (or (< r 0) (>= r h) (< c 0) (>= c w)))))
       (remove #(not-empty (get-in canvas %)))))

(defn run [path]
  (let [blizzards (read-blizzards path)
        h         (count blizzards)
        w         (count (first blizzards))
        top       [-1 0]
        bot       [h (dec w)]
        states    (cycle (take (lcm h w) (iterate update-blizzards blizzards)))
        bfs      (fn [start end minute]
                    (loop [minute  minute
                           all-pos #{start}]
                      (if (all-pos end)
                        minute
                        (let [canvas (first (drop (inc minute) states))]
                          (recur (inc minute) (set (mapcat #(neighbors % h w canvas) all-pos)))))))
        part1 (bfs top bot 0)
        part2 (->> part1
                   (bfs bot top)
                   (bfs top bot))]
    [part1 part2]))



(comment 
  (time (run "day24")) 
  )