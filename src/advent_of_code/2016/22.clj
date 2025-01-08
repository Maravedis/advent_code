(ns advent-of-code.2016.22
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2016 22))
(def tpath (u/test-path 2016 22))

(defn part1 [path]
  (let [input (->> (u/read-file-list path u/nums)
                   (drop 2))]
    (->> (for [i     (range (dec (count input)))
               j     (range (inc i) (count input))
               :let  [a (nth input i)
                      b (nth input j)]
               :when (or (and (not= 0 (a 3))
                              (<= (a 3) (b 4)))
                         (and (not= 0 (b 3))
                              (<= (b 3) (a 4))))]
           :pair)
         count)))

(defn print-map [path]
  (let [input (->> (u/read-file-list path u/nums)
                   (drop 2)
                   (mapcat (fn [[col row _ _ _ use%]]
                             [[row col] (cond (= use% 0) \_
                                              (> use% 95) \#
                                              :else \.)]))
                   (apply assoc {}))]
    (doseq [row (range 26)
            col (range 38)]
      (print (get input [row col]))
      (when (= col 37) (println)))))

;; Solution was visual, really too lazy to code a solution.
;; The full nodes form a "wall" that you only need to take into account moving to the top right.
;; Print the graph then:
;; 1. count how many steps to top right corner minus one column : 75
;; 2. go right once, moving the "payload" left : 1
;; 3. Count how many steps to go back to the left of the just moved node : 5
;; 4. Count how many steps to top left : 36, multiply it by the count on step 3.
(def part2 (+ 75 1 (* 36 5)))

(comment
  (part1 path)
  (print-map path)
  part2)
