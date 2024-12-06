(ns advent-of-code.2024.06
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2024 6))
(def tpath "2024/6_test.in")

(defn find-guard [grid]
  (ffirst (filter (fn [[_ v]] (= v \^)) grid)))

(defn patrol [grid [guard dir] seen]
  (loop [dir     dir
         cur     guard
         visited (transient (set seen))
         path    (transient [])]
    (if (not (grid cur))
      path
      (let [np (p/move cur dir)]
        (if (= \# (grid np))
          (if (visited [cur (p/turn-right dir)]) :loop (recur (p/turn-right dir) cur visited path))
          (recur dir np (conj! visited [cur dir]) (conj! path [cur dir])))))))

(defn part1 [path]
  (let [grid  (p/->grid (u/read-file-list path vec))
        guard (find-guard grid)]
    (->> (patrol grid [guard p/N] #{})
         persistent!
         (map first)
         set
         count)))

(defn part2 [path]
  (let [grid         (p/->grid (u/read-file-list path vec))
        guard        (find-guard grid)
        path         (->> (patrol grid [guard p/N] #{})
                          persistent!
                          vec)
        path-indexed (loop [seen      #{}
                            i         0
                            [[h] & t] path
                            acc       []]
                       (cond (nil? h) acc
                             (seen h) (recur seen (inc i) t (conj acc [i nil]))
                             :else (recur (conj seen h) (inc i) t (conj acc [i h]))))]
    (r/fold + (fn [acc [idx cur]]
                (cond-> acc (and cur
                                 (not= cur guard)
                                 (= :loop (patrol (assoc grid cur \#)
                                                  (get path (dec idx))
                                                  (subvec path 0 idx)))) inc)) path-indexed)))

(comment

  (time (part1 path))
  (time (part2 path))
  (time (part2 tpath)))