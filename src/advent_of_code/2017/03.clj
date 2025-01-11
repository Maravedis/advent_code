(ns advent-of-code.2017.03
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]))

(def path (u/get-input 2017 3))

(defn part1 [path]
  (let [x (u/read-file-line path parse-long)]
    (loop [pos    p/origin
           curr   1
           length 1
           dir    p/E]
      (cond (= curr x) (p/manhattan p/origin pos)
            (<= (+ curr length) x) (recur (p/move pos dir length)
                                          (+ curr length)
                                          (if (#{p/N p/S} dir) (inc length) length)
                                          (p/turn-left dir))
            :else (recur (p/move pos dir) (inc curr) length dir)))))

(defn part2 [path]
  (let [input (u/read-file-line path parse-long)]
    (loop [pos    [0 1]
           values (transient {[0 0] 1})
           dir    p/N]
      (let [value (reduce #(+ %1 (get values (p/move pos %2) 0)) 0 p/box)]
        (if (> value input)
          value
          (let [left (p/move pos (p/turn-left dir))]
            (if (values left)
              (recur (p/move pos dir) (assoc! values pos value) dir)
              (recur left (assoc! values pos value) (p/turn-left dir)))))))))

(comment
  (part1 path)
  (part2 path))
