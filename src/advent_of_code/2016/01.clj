(ns advent-of-code.2016.01
  (:require
   [advent-of-code.utils :as u]
   [advent-of-code.points :as p]))

(defn part1 [path]
  (let [input (->> (u/read-file-list path #(re-seq #"([RL])(\d+)" %))
                   first
                   (map (fn [[_ t n]] [(case t "R" p/turn-right "L" p/turn-left) (parse-long n)])))]
    (->> (reduce (fn [[curr dir] [f n]]
                   [(p/move curr (f dir) n) (f dir)]) [p/origin p/N] input)
         first
         (p/manhattan p/origin))))

(defn part2 [path]
  (let [input (->> (u/read-file-list path #(re-seq #"([RL])(\d+)" %))
                   first
                   (map (fn [[_ t n]] [(case t "R" p/turn-right "L" p/turn-left) (parse-long n)])))]
    (loop [curr        p/origin
           dir         p/N
           seen        #{}
           [[f n] & t] input]
      (let [ndir (f dir)
            line (take (inc n) (iterate #(p/move % ndir) curr))]
        (if-let [HQ (some seen (drop 1 line))]
          (p/manhattan p/origin HQ)
          (recur (last line) ndir (reduce conj seen line) t))))))
(comment
  (def path (u/get-input 2016 1))
  (def tpath "2016/1_test.in")

  (part1 path)
  (part2 path))