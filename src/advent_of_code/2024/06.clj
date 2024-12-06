(ns advent-of-code.2024.06
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [clojure.core.reducers :as r]))

(defn find-guard [grid]
  (ffirst (filter (fn [[_ v]] (= v \^)) grid)))

(def rotate {p/N p/E
             p/E p/S
             p/S p/W
             p/W p/N})

(defn patrol [grid guard]
  (loop [dir     p/N
         cur     guard
         visited (transient #{})]
    (if (not (grid cur))
      (into #{} (map first (persistent! visited)))
      (let [np (p/move cur dir)]
        (if (= \# (grid np))
          (if (visited [cur (rotate dir)]) :loop (recur (rotate dir) cur visited))
          (recur dir np (conj! visited [cur dir])))))))

(defn part1 [path]
  (let [grid  (p/->grid (u/read-file-list path vec))
        guard (find-guard grid)]
    (->> (patrol grid guard)
         count)))

(defn part2 [path]
  (let [grid  (p/->grid (u/read-file-list path vec))
        guard (find-guard grid)]
    (->> (disj (patrol grid guard) guard)
         vec
         (r/fold + #(if (= :loop (patrol (assoc grid %2 \#) guard)) (inc %1) %1)))))

(comment
  (def path (u/get-input 2024 6))
  (def tpath "2024/6_test.in")

  (time (part1 path))
  (time (part2 path)))