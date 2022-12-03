(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn read-file-list
  ([resource] (read-file-list resource read-string))
  ([resource modifier-fn]
   (let [list (slurp (io/resource resource))]
     (->> list
          str/split-lines
          (map modifier-fn)))))

(defn read-file-segmented-list
  ([resource] (read-file-segmented-list resource read-string))
  ([resource modifier-fn]
   (let [list (slurp (io/resource resource))]
     (->> (str/split list #"\n\n")
          (map str/split-lines)
          (map #(map modifier-fn %))))))

(defn sum [coll]
  (apply + coll))

(defn tprint [x]
  (pprint x)
  x)