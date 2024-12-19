(ns advent-of-code.2024.02
  (:require [advent-of-code.utils :as u]))

(defn safe? [report]
  (and (or (apply > report)
           (apply < report))
       (every? #(< 0 (abs (apply - %)) 4) (partition 2 1 report))))

(defn safe?2 [report]
  (some safe?
        (for [i (range 0 (count report))]
          (into (subvec report 0 i) (subvec report (inc i))))))

(defn part1 [path]
  (u/count-when safe? (u/read-file-list path u/nums)))

(defn part2 [path]
  (u/count-when safe?2 (u/read-file-list path u/nums)))

(comment
  (def path (u/get-input 2024 2))
  (def tpath (u/test-path 2024 2))

  (part1 path)
  (part1 tpath)
  (part2 path)
  (part2 tpath))