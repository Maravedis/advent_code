(ns advent-of-code.2024.18
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clojure.data.priority-map :refer [priority-map]]))

(defn make-memory [walls]
  (let [walls (set walls)]
    (->> (for [row (range 71)
               col (range 71)]
           [[col row] (if (walls [col row]) \# \.)])
         (into {}))))

(defn traceback [start end seen]
  (if (= end start)
    []
    (conj (traceback start (seen end) seen) end)))

(defn a-star [end grid]
  (loop [open      (priority-map p/origin 0)
         came-from {}
         gscore    {p/origin 0}]
    (if-let [curr (first (peek open))]
      (if (= end curr)
        came-from
        (let [tscore     (+ 1 (get gscore curr))
              neighbours (->> (p/neighbours curr #(when-let [x (grid %)] (not= \# x)))
                              (filter #(< tscore (get gscore % Integer/MAX_VALUE))))]
          (recur  (reduce #(apply assoc %1 %2) (pop open) (map #(vector % (+ tscore (p/manhattan end %))) neighbours))
                  (reduce #(apply assoc %1 %2) came-from (map #(vector % curr) neighbours))
                  (reduce #(apply assoc %1 %2) gscore (map #(vector % tscore) neighbours)))))
      :blocked)))

(defn part1 [path]
  (let [input (->> (u/read-file-list path u/nums)
                   (take 1024))
        end   [70 70]]
    (->> (make-memory input)
         (a-star end)
         (traceback p/origin end)
         (count))))

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
              result         (a-star end memory-blocked)]
          (if (= result :blocked)
            (recur lower (quot (+ upper lower) 2))
            (recur upper (- (* 2 upper) lower))))))))

(comment
  (def path (u/get-input 2024 18))
  (def tpath "2024/18_test.in")

  (time (part1 path))
  (time (part2 path)))
