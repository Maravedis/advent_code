(ns advent-of-code.2024.12
  (:require
   [advent-of-code.points :as p]
   [advent-of-code.utils :as u]
   [clojure.core.reducers :as r]))

(defn parse-input [path]
  (loop [regions (transient [])
         grid    (p/->grid (u/read-file-list path vec))]
    (if (empty? grid)
      (persistent! regions)
      (let [[start v]   (first grid)
            next-region (p/flood-fill grid start #(= v (grid %)))]
        (recur (conj! regions next-region)
               (apply dissoc grid next-region))))))

(defn perimeter [region]
  (r/fold + (fn [acc point]
              (+ acc (->> (p/neighbours point)
                          (remove region)
                          count))) region))

(defn part1 [path]
  (let [regions (parse-input path)]
    (r/fold + #(+ %1 (* (perimeter %2) (count %2))) regions)))

; part2

(defn walk-edge [region point dir]
  (let [left  (p/turn-left dir)
        right (p/turn-right dir)]
    (loop [prev point
           curr (p/move point dir)
           i    0]
      (cond (nil? (region curr))        [prev (if (region (p/move prev left)) left right)]
            (region (p/move curr left)) [curr left]
            :else (recur curr (p/move prev dir) (inc i))))))

(defn calc-internal [region outer-edge]
  (loop [points (->> (r/mapcat #(p/neighbours %) region)
                     (r/remove region)
                     (r/remove outer-edge)
                     (into #{}))
         acc    []]
    (if (empty? points)
      acc
      (let [island (p/flood-fill points (first points) (constantly true))]
        (recur (apply disj points island)
               (conj acc island))))))

(defn perimeter-side [region]
  (loop [acc        0
         dir        p/E
         curr       (first (sort region))
         seen       (transient #{})
         outer-edge (transient #{(p/move curr p/N)})]
    (let [[edge-end next-dir] (walk-edge region curr dir)]
      (if (seen [edge-end next-dir])
        (if-let [internals (seq (calc-internal region (reduce disj! outer-edge region)))]
          (apply + acc (map perimeter-side internals))
          acc)
        (recur (+ acc 1)
               next-dir edge-end
               (conj! seen [edge-end next-dir])
               (reduce conj! outer-edge (p/all-points-between (p/move curr (p/turn-left dir)) (p/move edge-end (p/turn-left dir)) dir)))))))

(defn part2 [path]
  (let [regions (parse-input path)]
    (r/fold + (fn [acc region] (+ acc (* (perimeter-side region) (count region)))) regions)))

(comment
  (def path (u/get-input 2024 12))
  (def tpath "2024/12_test.in")

  ; part1 1488414
  ; part2 911750

  (time (part1 path))
  (time (part2 path)))