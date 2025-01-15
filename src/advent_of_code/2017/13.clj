(ns advent-of-code.2017.13
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 13))
(def tpath (u/test-path 2017 13))

(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       (reduce (fn [acc [depth range]]
                 (cond-> acc
                   (= 0 (mod depth (* 2 (- range 1)))) (+ (* range depth)))) 0)))

(defn part2 [path]
  (let [firewall (u/read-file-list path u/nums)]
    (loop [i 0]
      (if (reduce (fn [_ [depth range]]
                    (if (= 0 (mod (+ i depth) (* 2 (- range 1))))
                      (reduced false)
                      true)) true firewall)
        i
        (recur (inc i))))))

(comment

  (part1 tpath)
  (part1 path)
  (part2 tpath)
  (part2 path)

  ;
  )
