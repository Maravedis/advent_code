(ns advent-of-code.2015.21
  (:require [advent-of-code.utils :as u]
            [clojure.math.combinatorics :as c]))

(def path (u/get-input 2015 21))
(def boss (mapv first (u/read-file-list path u/nums)))

(def weapons [[8  4 0]
              [10 5 0]
              [25 6 0]
              [40 7 0]
              [74 8 0]])

(def armors [[0   0 0]
             [13  0 1]
             [31  0 2]
             [53  0 3]
             [75  0 4]
             [102 0 5]])

(def rings [[0   0 0]
            [0   0 0]
            [25  1 0]
            [50  2 0]
            [100 3 0]
            [20  0 1]
            [40  0 2]
            [80  0 3]])

(defn blow [[hp a def]  [_ atk _]]
  [(- hp (max 1 (- atk def))) a def])

(defn simulate [player boss]
  (loop [[hpp :as player] player
         [hpb :as boss]   boss
         turn             0]
    (cond (<= hpp 0) :boss
          (<= hpb 0) :player
          (even? turn) (recur player (blow boss player) (inc turn))
          (odd? turn) (recur (blow player boss) boss (inc turn)))))

(defn add-item [player [_ atk def]]
  (-> player
      (update 1 + atk)
      (update 2 + def)))

(defn solve [winner]
  (->> (for [weapon weapons
             armor  armors
             rings  (c/combinations rings 2)
             :let   [items (concat [weapon armor] rings)
                     player (reduce add-item [100 0 0] items)
                     cost (reduce #(+ %1 (first %2)) 0 items)]
             :when  (= winner (simulate player boss))]
         cost)
       (apply (if (= winner :player) min max))))

(comment
  (solve :player)
  (solve :boss))