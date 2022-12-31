(ns advent-of-code.2015.05
  (:require [advent-of-code.utils :as u]))

(def naughty-block (set (map seq ["ab" "cd" "pq" "xy"])))

(defn is-nice1? [line]
  (and (>= (count (filter (fn [c] (#{\a \e \i \o \u} c)) line)) 3)
       (->> (partition 2 1 line)
            (reduce (fn [acc [a b :as block]]
                      (if (naughty-block block)
                        (reduced false)
                        (or acc (= a b)))) false))))

(defn is-nice2? [line]
  (and (->> (partition 3 1 line)
            (drop-while (fn [[a _ c]] (not= a c)))
            first
            some?)
       (->> (partition 2 1 line)
            (reduce (fn [[acc alt] i]
                      (if (and alt (= i (peek acc)))
                        [acc false]
                        [(conj acc i) true])) [[] true])
            first
            (group-by identity)
            (some (fn [[_ v]] (<= 2 (count v)))))))

(defn part1 [path]
  (u/count-if is-nice1? (u/read-file-list path identity)))

(defn part2 [path]
  (u/count-if is-nice2? (u/read-file-list path identity)))

(comment
  (u/get-input 2015 5)
  (part1 "2015/5.in")
  (part2 "2015/5.in")
  )