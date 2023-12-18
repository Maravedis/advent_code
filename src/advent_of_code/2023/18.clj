(ns advent-of-code.2023.18
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]))

(defn parse-line [line]
  (let [[dir len] (->> (split line #" ")
                       (take 2))]
    [({"R" 0 "D" 1 "L" 2 "U" 3} dir) (parse-long len)]))

(defn parse-color [line]
  (let [[_ len dir]  (re-find #"([a-f0-9]{2,})(\d)" line)]
    [(parse-long dir) (Integer/parseInt len 16)]))

(defn gen-solve [instructions]
  (let [points (loop [[r c :as curr]       [0 0]
                      [[dir n :as in] & t] instructions
                      res                  (transient [])]
                 (if (nil? in)
                   (persistent! (conj! res curr))
                   (recur (case dir
                            0 [r (+ c n)]
                            1 [(+ r  n) c]
                            2 [r (- c n)]
                            3 [(- r n) c])
                          t (conj! res curr))))]
    (u/area-vertices points)))

(defn part1 [path]
  (let [input  (u/read-file-list path parse-line)]
    (gen-solve input)))

(defn part2 [path]
  (let [input  (u/read-file-list path parse-color)]
    (gen-solve input)))

(comment
  (def path (u/get-input 2023 18))
  (def tpath "2023/18_test.in")

  (part1 path)
  (part2 path)
  )