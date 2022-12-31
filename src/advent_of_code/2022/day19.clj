(ns advent-of-code.2022.day19
  (:require [advent-of-code.utils :as u]))

(defn read-blueprint [line]
  (->> (u/nums line) (drop 1) vec))

(defn calc-geodes [minutes blueprint]
  (let [max_clay  (blueprint 3)
        max_ore   (max (blueprint 0) (blueprint 1) (blueprint 2) (blueprint 4))
        clay-cond (fn [ore clay-r] (and (>= ore (blueprint 1)) (< clay-r max_clay)))
        ore-cond  (fn [ore ore-r] (and (>= ore (blueprint 0)) (< ore-r max_ore)))
        dfs       (fn [dfs ore clay obs geodes ore-r clay-r obs-r geo-r minute]
                    (if (>= minute minutes)
                      geodes
                      (cond
                        (and (>= ore (blueprint 4)) (>= obs (blueprint 5))) (dfs (+ (- ore (blueprint 4)) ore-r)
                                                                                 (+ clay clay-r)
                                                                                 (+ (- obs (blueprint 5)) obs-r)
                                                                                 (+ geodes geo-r)
                                                                                 ore-r clay-r obs-r (inc geo-r) (inc minute))
                        (and (>= ore (blueprint 2)) (>= clay (blueprint 3))) (dfs (+ (- ore (blueprint 2)) ore-r)
                                                                                  (+ (- clay (blueprint 3)) clay-r)
                                                                                  (+ obs obs-r)
                                                                                  (+ geodes geo-r)
                                                                                  ore-r clay-r (inc obs-r) geo-r (inc minute))
                        (and (clay-cond ore clay-r) (ore-cond ore ore-r)) (max (dfs (+ (- ore (blueprint 1)) ore-r)
                                                                                    (+ clay clay-r)
                                                                                    (+ obs obs-r)
                                                                                    (+ geodes geo-r)
                                                                                    ore-r (inc clay-r) obs-r geo-r (inc minute))
                                                                               (dfs (+ (- ore (blueprint 0)) ore-r)
                                                                                    (+ clay clay-r)
                                                                                    (+ obs obs-r)
                                                                                    (+ geodes geo-r)
                                                                                    (inc ore-r) clay-r obs-r geo-r (inc minute))
                                                                               (dfs (+ ore ore-r)
                                                                                    (+ clay clay-r)
                                                                                    (+ obs obs-r)
                                                                                    (+ geodes geo-r)
                                                                                    ore-r clay-r obs-r geo-r (inc minute)))
                        (clay-cond ore clay-r) (max (dfs (+ (- ore (blueprint 1)) ore-r)
                                                         (+ clay clay-r)
                                                         (+ obs obs-r)
                                                         (+ geodes geo-r)
                                                         ore-r (inc clay-r) obs-r geo-r (inc minute))
                                                    (dfs (+ ore ore-r)
                                                         (+ clay clay-r)
                                                         (+ obs obs-r)
                                                         (+ geodes geo-r)
                                                         ore-r clay-r obs-r geo-r (inc minute)))
                        (ore-cond ore ore-r) (max (dfs (+ (- ore (blueprint 0)) ore-r)
                                                       (+ clay clay-r)
                                                       (+ obs obs-r)
                                                       (+ geodes geo-r)
                                                       (inc ore-r) clay-r obs-r geo-r (inc minute))
                                                  (dfs (+ ore ore-r)
                                                       (+ clay clay-r)
                                                       (+ obs obs-r)
                                                       (+ geodes geo-r)
                                                       ore-r clay-r obs-r geo-r (inc minute)))
                        :else (dfs (+ ore ore-r)
                                   (+ clay clay-r)
                                   (+ obs obs-r)
                                   (+ geodes geo-r)
                                   ore-r clay-r obs-r geo-r (inc minute)))))
        dfs-m     (u/fix (memoize dfs))]
    (dfs-m 0 0 0 0 1 0 0 0 0)))

(defn part1 [path]
  (->> (u/read-file-list path read-blueprint)
       (pmap (partial calc-geodes 24))
       (map-indexed (fn [idx i] (* (inc idx) i)))
       (reduce +)))

(defn part2 [path]
  (->> (u/read-file-list path read-blueprint)
       (take 3)
       (pmap (partial calc-geodes 32))
       (reduce *)))


(comment 
  
  (time (part1 "2022/day19"))
  (time (part2 "2022/day19"))
  )