(ns advent-of-code.2024.22
  (:require [advent-of-code.utils :as u]
            [clojure.core.reducers :as r]))

(def path (u/get-input 2024 22))
(def tpath (u/test-path 2024 22))

(defn mix-prune [value secret-number]
  (mod (bit-xor value secret-number) 16777216))

(defn next-step [number]
  (let [a (mix-prune (* 64 number) number)
        b (mix-prune (quot a 32) a)]
    (mix-prune (* 2048 b) b)))

(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       (r/map first)
       (r/map #(nth (iterate next-step %) 2000))
       (r/fold +)))

(defn part2 [path]
  (->> (vec (u/read-file-list path u/nums))
       (r/map first)
       (r/map (fn [buyer]
                (->> (take 2001 (iterate next-step buyer))
                     (map #(mod % 10))
                     (partition 5 1)
                     (map (fn [[a b c d price]] [[(- b a) (- c b) (- d c) (- price d)] price]))
                     (reduce (fn [acc [sequence price]] (update acc sequence #(or % price))) {}))))
       (r/fold (r/monoid #(merge-with + %1 %2) (constantly {})))
       vals
       (apply max)))

(comment

  (time (part1 path))
  (time (part2 path)))
