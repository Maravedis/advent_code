(ns advent-of-code.2024.18
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]))

(defn make-memory [walls]
  (let [walls (set walls)]
    (->> (for [row (range 71)
               col (range 71)]
           [[col row] (if (walls [col row]) \# \.)])
         (into {}))))

(defn part1 [path]
  (let [input (->> (u/read-file-list path u/nums)
                   (take 1024))
        end   [70 70]]
    (-> (make-memory input)
        (p/a-star-score p/origin end))))

(defn part2 [path]
  (let [[input blocks] (->> (u/read-file-list path u/nums)
                            (split-at 1024))
        end            [70 70]
        memory         (make-memory input)]
    (loop [lower 0
           upper (dec (count blocks))]
      (if (= upper lower)
        (nth blocks upper)
        (let [memory-blocked (reduce #(assoc %1 %2 \#) memory (take upper blocks))
              result         (p/a-star-score memory-blocked p/origin end)]
          (if (= result :blocked)
            (recur lower (quot (+ upper lower) 2))
            (recur upper (- (* 2 upper) lower))))))))

(comment
  (def path (u/get-input 2024 18))
  (def tpath (u/test-path 2024 18))

  (time (part1 path))
  (time (part2 path)))
