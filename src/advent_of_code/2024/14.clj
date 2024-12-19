(ns advent-of-code.2024.14
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn calc-quadrants [h w robots]
  (->> (reduce (fn [acc [x y]]
                 (cond-> acc
                   (and (< x (quot w 2)) (< y (quot h 2))) (update 0 inc)
                   (and (> x (quot w 2)) (< y (quot h 2))) (update 1 inc)
                   (and (< x (quot w 2)) (> y (quot h 2))) (update 2 inc)
                   (and (> x (quot w 2)) (> y (quot h 2))) (update 3 inc))) [0 0 0 0] robots)))

(defn print-robots [h w robots]
  (->> (for [r (range 0 h)
             c (range 0 (inc w))]
         (let [x (u/count-when #(= [c r] (take 2 %)) robots)]
           (cond (= c w) \newline
                 (> x 0) (char (+ x 48))
                 :else \.)))
       str/join
       (spit (str "out"))))

(defn next-state [robots h w]
  (map (fn [[x y dx dy]] [(mod (+ x dx) w) (mod (+ y dy) h) dx dy]) robots))

(defn part1 [path h w]
  (let [robots (u/read-file-list path u/nums)]
    (->> (nth (iterate #(next-state % h w) robots) 100)
         (calc-quadrants h w)
         (reduce *))))

(defn part2 [path h w]
  (let [robots (u/read-file-list path u/nums)
        states (iterate #(next-state % h w) robots)]
    (->> states
         (keep-indexed (fn [seconds robots]
                         (when (some #(>= % 250) (calc-quadrants h w robots))
                           #_(print-robots h w robots)
                           seconds)))
         first)))

(comment
  (def path (u/get-input 2024 14))
  (def tpath (u/test-path 2024 14))

  (part1 tpath 11 7)
  (part1 path 103 101)
  (part2 path 103 101))