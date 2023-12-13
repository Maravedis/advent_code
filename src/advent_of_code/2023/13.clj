(ns advent-of-code.2023.13
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [intersection]]))

(defn check-symetry [val i]
  (let [[fi se] (split-at i val)
        coll    (map = (reverse fi) se)]
    (not (some not coll))))

(defn find-vertical [input prev]
  (let [w (count (first input))]
    (->> (map (fn [line] (set (for [i     (range 1 w)
                                    :when (and (not= i prev) (check-symetry line i))]
                                i))) input)
         (apply intersection)
         first)))

(defn find-horizontal [input prev]
  (loop [[i & t] (range 1 (count input))]
    (if (or (nil? i) (and (check-symetry input i) (not= (* 100 i) prev)))
      (and i (* 100 i))
      (recur t))))

(defn calc-block
  ([block] (calc-block block nil))
  ([block prev]
   (if-let [i (find-horizontal block prev)]
     i
     (if-let [j (find-vertical block prev)]
       j
       nil))))

(defn part1 [path]
  (let [input (u/read-file-segmented-list path seq)]
    (loop [[block & t] input
           result 0]
      (if (nil? block)
        result
        (recur t (+ result (calc-block block)))))))

(def sym-map {\. \# \# \.})

(defn part2 [path]
  (let [input (vec (u/read-file-segmented-list path vec))]
    (loop [[block & t] input
           result      0]
      (if (nil? block)
        result
        (let [pr (calc-block block)]
          (recur t (+ result (first (for [i     (range 0 (count block))
                                          j     (range 0 (count (first block)))
                                          :let  [v (calc-block (update-in block [i j] sym-map) pr)]
                                          :when v] v)))))))))

(comment

  (def path (u/get-input 2023 13))
  (def tpath "2023/13_test.in")
  (time (part1 path))
  (time (part2 path))
  )