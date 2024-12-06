(ns advent-of-code.2024.06
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [clojure.core.reducers :as r]))

(defn find-guard [grid]
  (first
   (for [r     (range (count grid))
         c     (range (count (first grid)))
         :when (= \^ (get-in grid [r c]))]
     [r c])))

(def rotate {p/N p/E
             p/E p/S
             p/S p/W
             p/W p/N})

(defn initial [grid guard]
  (let [v-edge #{0 (dec (count grid))}
        h-edge #{0 (dec (count (first grid)))}]
    (loop [dir           p/N
           [r c :as cur] guard
           visited       #{guard}]
      (if (or (v-edge r) (h-edge c))
        visited
        (let [np (p/move cur dir)]
          (if (= \# (get-in grid np))
            (recur (rotate dir) cur visited)
            (recur dir np (conj visited np))))))))

(defn part1 [path]
  (let [grid  (vec (u/read-file-list path vec))
        guard (find-guard grid)]
    (count (initial grid guard))))

(defn loop? [guard grid]
  (loop [dir     p/N
         cur     guard
         visited {guard #{dir}}]
    (if (not (get-in grid cur))
      false
      (let [np (p/move cur dir)]
        (if (= \# (get-in grid np))
          (or ((get visited cur #{}) (rotate dir))
              (recur (rotate dir) cur visited))
          (recur dir np (update visited cur (fnil conj #{}) dir)))))))

(defn part2 [path]
  (let [grid  (vec (u/read-file-list path vec))
        guard (find-guard grid)]
    (->> (disj (initial grid guard) guard)
         vec
         (r/fold + #(if (loop? guard (assoc-in grid %2 \#)) (inc %1) %1)))))

(comment
  (def path (u/get-input 2024 6))
  (def tpath "2024/6_test.in")

  (time (part1 path))
  (time (part2 path)))