(ns advent-of-code.2016.19
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2016 19))
(def tpath (u/test-path 2016 19))

(defn part1
  ([path] (let [elves (u/read-file-line path parse-long)] (part1 elves 1 elves 2)))
  ([elves first-elf last-elf step]
   (if (= elves 1)
     first-elf
     (if (odd? elves)
       (recur (quot elves 2) (+ first-elf step) last-elf (* 2 step))
       (recur (quot elves 2) first-elf (- last-elf step) (* 2 step))))))

; I truly have no idea how this is supposed to work. I worked out the numbers by doing a naive implementation and comparing things.
(defn part2 ([path] (let [elves (u/read-file-line path parse-long)] (part2 elves 1 elves 2)))
  ([n first-elf last-elf step]
   (cond (= n 2) first-elf
         (= n 3) last-elf
         :else (case (mod n 3)
                 0 (recur (quot n 3) (+ first-elf step) last-elf (* step 3))
                 1 (recur (inc (quot n 3)) first-elf (- last-elf step) (* step 3))
                 2 (recur (inc (quot n 3)) (+ first-elf (quot step 2)) (- last-elf (quot step 2)) (* step 3))))))

(comment
  (part1 tpath)
  (time (part1 path))
  (part2 tpath)
  (time (part2 path)))
