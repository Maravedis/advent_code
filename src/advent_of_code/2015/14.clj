(ns advent-of-code.2015.14
  (:require [advent-of-code.utils :as u]))

(defn calc [race [speed duration rest]]
  (let [comb (+ duration rest)]
    (+ (* speed (quot race comb) duration)
       (* speed (min duration (mod race comb))))))

(defn part1 [path]
  (->> (u/read-file-list path u/nums)
       (map #(calc 2503 %))
       (apply max)))

(defn part2 [path]
  (let [input (u/read-file-list path u/nums)]
    (loop [result (vec (repeat (count input) 0))
           i      1]
      (if (> i 2503)
        (apply max result)
        (let [races  (map #(calc i %) input)
              winner (u/index-max races)]
          (recur (update result winner inc) (inc i)))))))

(comment
  (def path (u/get-input 2015 14))

  (part1 path)
  (part2 path))