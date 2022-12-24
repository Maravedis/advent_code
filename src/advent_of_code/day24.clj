(ns advent-of-code.day24
  (:require [advent-of-code.utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn run [path part1?]
  (let [blizzards (read-blizzards path)
        h         (count blizzards)
        w         (count (first blizzards))
        top       [-1 0]
        bot       [h (dec w)]
        states    (iterate update-blizzards blizzards)
        queue     (atom #{})
        bfs      (fn [start end minute]
                    (reset! queue #{start})
                    (loop [minute minute]
                      (let [all-pos @queue]
                        (if (all-pos end)
                          minute
                          (let [canvas (first (drop (inc minute) states))]
                            (reset! queue (set (mapcat #(neighbors % h w canvas) all-pos)))
                            (recur (inc minute)))))))]
    (if part1?
      (bfs top bot 0)
      (->> (bfs top bot 0)
           (bfs bot top)
           (bfs top bot)))))



(comment 
  (time (run "day24" true))
  (time (run "day24" false)) 
  )