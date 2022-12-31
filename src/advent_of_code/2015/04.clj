(ns advent-of-code.2015.04
  (:require [advent-of-code.utils :as u]
            [clj-commons.digest :refer [md5]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn part1 [path]
  (let [key (slurp (io/resource path))]
    (loop [[h & r] (map #(str key %) (range Integer/MAX_VALUE))]
      (let [x (md5 h)]
        (if (str/starts-with? x "00000")
          (subs h (count key))
          (recur r))))))

(defn part2 [path]
  (let [key (slurp (io/resource path))]
    (loop [[h & r] (map #(str key %) (range Integer/MAX_VALUE))]
      (let [x (md5 h)]
        (if (str/starts-with? x "000000")
          (subs h (count key))
          (recur r))))))

(comment
  (u/get-input 2015 4)

  (part1 "2015/4.in")
  (part2 "2015/4.in")
  )