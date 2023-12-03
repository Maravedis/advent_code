(ns advent-of-code.2023.day3
  (:require [advent-of-code.utils :as u]))

(defn read-schematics [path]
  (let [mtx (u/read-file-list path vec)]
    (loop [[line & t] mtx
           i          0
           nums       {}
           syms       {}]
      (if (nil? line)
        [nums syms]
        (let [[_ n s] (reduce-kv (fn [[buf n s] j x]
                                   (cond
                                     (and (not (u/char->digit x)) (not-empty buf)) [""
                                                                                    (assoc n [i (- j (count buf)) (count buf)] (read-string buf))
                                                                                    (cond-> s (not= \. x) (assoc [i j] x))]
                                     (u/char->digit x) [(str buf x) n s]
                                     (not= \. x) [buf n (assoc s [i j] x)]
                                     :else [buf n s]))
                                 ["" {} {}] (conj line \.))]
          (recur t (inc i) (merge nums n) (merge syms s)))))))

(defn in-nums [nums i j]
  (keep (fn [[[x y l] num]] (when (and (<= (dec i) x (inc i)) (<= (dec y) j (+ y l))) num)) nums))

(defn part1 [path]
  (let [[nums syms] (read-schematics path)]
    (reduce (fn [result [[i j]]]
              (apply + result (in-nums nums i j))) 
            0 syms)))

(defn part2 [path]
  (let [[nums syms] (read-schematics path)]
    (reduce (fn [result [[i j] sym]]
              (if (= sym \*)
                (let [out (in-nums nums i j)]
                  (if (= 2 (count out)) (+ result (apply * out)) result))
                result)) 0 syms)))


(comment 
  (time (part1 "2023/3.in")) 
  (time (part2 "2023/3.in"))
  )