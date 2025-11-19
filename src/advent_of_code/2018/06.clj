(ns advent-of-code.2018.06
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [com.rpl.specter :as sp]))

(def path (u/get-input 2018 6))
(def tpath (u/test-path 2018 6))

(defn flood [a-grid a-counters [idx start]]
  (loop [grid     a-grid
         counters a-counters
         [h & t]  (set (p/neighbours start))
         visited  #{start}]
    (if h
      (let [dist             (p/manhattan start h)
            [jdx djst :as j] (get grid h)
            neighbors        (p/neighbours h #(and (< dist 75) (not (contains? visited %))))]
        (cond
          (visited h) (recur grid counters t visited)
          (not j) (recur (assoc grid h [idx dist])
                         (update counters idx inc)
                         (reduce conj t neighbors)
                         (conj visited h))
          (< dist djst) (recur (assoc grid h [idx dist])
                               (cond-> (update counters idx inc)
                                 (not= \= jdx) (update jdx dec))
                               (reduce conj t neighbors)
                               (conj visited h))
          (= dist djst) (do
                          (recur (assoc grid h [\= dist])
                                 (cond-> counters
                                   (not= \= jdx) (update jdx dec))
                                 t
                                 (conj visited h)))
          (> dist djst) (recur grid counters t (conj visited h))))
      [grid counters])))

; ran it with flood on 200 and 100 and took the highest value that didn't move.
(defn part1 [path]
  (let [input (->> (u/read-file-list path u/nums)
                   (map-indexed vector)
                   vec)]
    (loop [[h & t]  input
           grid     (->> (for [[idx p] input] [p [idx 0]])
                         (into {}))
           counters (->> (for [i (range (count input))] [i 1])
                         (into {}))]
      (if (not h)
        (sort-by second counters)
        #_(println (sp/transform [sp/MAP-VALS] (constantly \A) grid))
        (let [[n-grid n-counter] (flood grid counters h)]
          (recur t n-grid n-counter))))))

(comment
  (let [input (->> (u/read-file-list path u/nums)
                   (map-indexed vector)
                   vec)]
    (loop [[h & t]  input
           grid     (->> (for [[idx p] input] [p [idx 0]])
                         (into {}))
           counters (->> (for [i (range (count input))] [i 1])
                         (into {}))]
      (if (not h)
        (sort-by second counters)
        #_(println (sp/transform [sp/MAP-VALS] (constantly \A) grid))
        (let [[n-grid n-counter] (flood grid counters h)]
          (recur t n-grid n-counter))))))