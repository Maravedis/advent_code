(ns advent-of-code.2015.04
  (:require [advent-of-code.utils :as u]
            [clj-commons.digest :refer [md5]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn calc [path starts]
  (let [key (slurp (io/resource path))]
    (->> (range Integer/MAX_VALUE)
         (drop-while #(not (str/starts-with? (md5 (str key %)) starts)))
         first)))

(comment
  (u/get-input 2015 4)

  (calc "2015/4.in" "00000")
  (calc "2015/4.in" "000000")
  )