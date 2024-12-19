(ns advent-of-code.2016.05
  (:require [advent-of-code.utils :as u]
            [clj-commons.digest :refer [md5]]
            [clojure.string :refer [join]]))

(def path (u/get-input 2016 05))
(def tpath (u/test-path 2016 05))

(defn part1 [path]
  (let [input (->> (u/read-file-list path identity)
                   first)]
    (loop [idx      0
           password []]
      (if (= (count password) 8)
        (join password)
        (let [hash (md5 (str input idx))]
          (if (= "00000" (subs hash 0 5))
            (recur (inc idx) (conj password (.charAt hash 5)))
            (recur (inc idx) password)))))))

(defn part2 [path]
  (let [input (->> (u/read-file-list path identity)
                   first)]
    (loop [idx      0
           password {}]
      (if (= (count password) 8)
        (join (for [i (range 8)] (password i)))
        (let [hash (md5 (str input idx))]
          (if (= "00000" (subs hash 0 5))
            (let [i (u/char->digit (.charAt hash 5))
                  v (.charAt hash 6)]
              (if (and i (< i 8) (not (password i)))
                (recur (inc idx) (assoc password i v))
                (recur (inc idx) password)))
            (recur (inc idx) password)))))))
