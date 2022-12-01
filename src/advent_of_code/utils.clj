(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-file-list
  ([resource] (read-file-list resource read-string))
  ([resource modifier-fn]
   (let [list (slurp (io/resource resource))]
     (->> (str/split list #"\n\n")
          (map str/split-lines)
          (map #(map modifier-fn %))))))