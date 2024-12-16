(ns advent-of-code.2024.15
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [clojure.core.reducers :as r]))

(defn parse-map [rows]
  (let [grid  (p/->grid rows)
        start (ffirst (filter (fn [[_ v]] (= v \@)) grid))
        boxes (->> (filter (fn [[_ v]] (= v \O)) grid)
                   (map first)
                   (set))
        walls (->> (filter (fn [[_ v]] (= v \#)) grid)
                   (map first)
                   (set))]
    [start boxes walls]))

(defn move-thing [pos boxes walls dir]
  (let [next-pos                     (p/move pos dir)
        [moved-boxes [next-non-box]] (split-with boxes (iterate #(p/move % dir) next-pos))]
    (cond (walls next-non-box) [pos boxes]
          (= next-pos next-non-box) [next-pos boxes]
          :else [next-pos (-> (disj boxes (first moved-boxes))
                              (conj next-non-box))])))

(defn part1 [path]
  (let [[space ins]         (u/read-file-segmented-list path vec)
        [start boxes walls] (parse-map space)]
    (loop [[h & t] (mapcat #(map p/caret-dirs %) ins)
           curr    start
           boxes   boxes]
      (if  (nil? h)
        (r/fold + (fn [acc [r c]] (+ acc (* 100 r) c)) boxes)
        (let [[next-pos new-boxes] (move-thing curr boxes walls h)]
          (recur t next-pos new-boxes))))))

(defn parse-map-wide [rows]
  (let [grid        (->> (for [row (range 0 (count rows))
                               col (range 0 (* 2 (count (first rows))) 2)]
                           (case (get-in rows [row (/ col 2)])
                             \# [[row col] \# [row (+ 1 col)] \#]
                             \O [[row col] \[ [row (+ 1 col)] \]]
                             \@ [[row col] \@ [row (+ 1 col)] \.]
                             \. [[row col] \. [row (+ 1 col)] \.]))
                         (r/fold (r/monoid merge hash-map) (fn [acc x] (apply assoc acc x))))
        start       (ffirst (filter (fn [[_ v]] (= v \@)) grid))
        left-boxes  (->> (filter (fn [[_ v]] (= \[ v)) grid)
                         (map first)
                         (set))
        right-boxes (->> (filter (fn [[_ v]] (= \] v)) grid)
                         (map first)
                         (set))
        walls       (->> (filter (fn [[_ v]] (= v \#)) grid)
                         (map first)
                         (set))]
    [start left-boxes right-boxes walls]))

(defn move-wide-horizontal [pos left-boxes right-boxes walls dir]
  (let [next-pos               (p/move pos dir)
        [boxes [next-non-box]] (split-with #(or (left-boxes %) (right-boxes %)) (iterate #(p/move % dir) next-pos))
        [lb rb]                (->> (partition 2 (if (= p/W dir) (reverse boxes) boxes))
                                    ((juxt #(map first %) #(map second %))))]
    (if (walls next-non-box)
      [pos left-boxes right-boxes]
      [next-pos
       (apply conj (apply disj left-boxes lb) (map #(p/move % dir) lb))
       (apply conj (apply disj right-boxes rb) (map #(p/move % dir) rb))])))

(defn move-wide-vertical [pos left-boxes right-boxes walls dir]
  (let [next-pos (p/move pos dir)]
    (cond (walls next-pos) [pos left-boxes right-boxes]
          (not (or (left-boxes next-pos) (right-boxes next-pos))) [next-pos left-boxes right-boxes]
          :else
          (loop [open   [(or (left-boxes next-pos) (p/move next-pos p/W))
                         (or (right-boxes next-pos) (p/move next-pos p/E))]
                 lefts  #{}
                 rights #{}]
            (cond (some walls open) [pos left-boxes right-boxes]
                  (empty? open) [next-pos
                                 (apply conj (apply disj left-boxes lefts) (map #(p/move % dir) lefts))
                                 (apply conj (apply disj right-boxes rights) (map #(p/move % dir) rights))]
                  :else
                  (let [next-row (map #(p/move % dir) open)]
                    (recur (mapcat (fn [side] (cond (left-boxes side) [side (p/move side p/E)]
                                                    (right-boxes side) [(p/move side p/W) side]
                                                    (walls side) [side]
                                                    :else [])) next-row)
                           (apply conj lefts (filter left-boxes open))
                           (apply conj rights (filter right-boxes open)))))))))

(defn move-wide-things [pos left-boxes right-boxes walls dir]
  (condp contains? dir
    #{p/W p/E} (move-wide-horizontal pos left-boxes right-boxes walls dir)
    #{p/N p/S} (move-wide-vertical pos left-boxes right-boxes walls dir)))

(defn part2 [path]
  (let [[space ins]                          (u/read-file-segmented-list path vec)
        [start left-boxes right-boxes walls] (parse-map-wide space)]
    (loop [[h & t] (mapcat #(map p/caret-dirs %) ins)
           curr    start
           lboxes  left-boxes
           rboxes  right-boxes]
      (if (nil? h)
        (r/fold + (fn [acc [r c]] (+ acc (* 100 r) c)) lboxes)
        (let [[next-pos new-l-boxes new-r-boxes] (move-wide-things curr lboxes rboxes walls h)]
          (recur t next-pos new-l-boxes new-r-boxes))))))

(comment

  (def path (u/get-input 2024 15))
  (def tpath "2024/15_test.in")

  (part1 path)
  (part2 path))