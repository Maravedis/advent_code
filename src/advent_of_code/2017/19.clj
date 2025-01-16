(ns advent-of-code.2017.19
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [clojure.string :refer [join]]))

(def path (u/get-input 2017 19))
(def tpath (u/test-path 2017 19))

(def letters (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn run [path]
  (let [grid  (->> (u/read-file-list path vec)
                   vec
                   (reduce-kv (fn [acc r line]
                                (reduce-kv (fn [acc c v]
                                             (cond-> acc (not= \space v) (assoc [r c] v))) acc line)) {}))
        start (ffirst (filter (fn [[[r] v]] (and (= r 0) (= v \|))) grid))]
    (loop [pos    start
           dir    p/S
           output []
           steps  1]
      (let [forward (p/move pos dir)]
        (if (grid forward)
          (recur forward dir (cond-> output (letters (grid pos)) (conj (grid pos))) (inc steps))
          (if (= \+ (grid pos))
            (let [right (p/move pos (p/turn-right dir))
                  left  (p/move pos (p/turn-left dir))]
              (cond (#{p/N p/S} dir) (if (and (grid right) (not= \| (grid right)))
                                       (recur right (p/turn-right dir) output (inc steps))
                                       (recur left (p/turn-left dir) output (inc steps)))
                    (#{p/E p/W} dir) (if (and (grid right) (not= \- (grid right)))
                                       (recur right (p/turn-right dir) output (inc steps))
                                       (recur left (p/turn-left dir) output (inc steps)))))
            [steps (cond-> output
                     (letters (grid pos)) (conj (grid pos))
                     :always (join))]))))))

(comment
  (run tpath)
  (run path))
