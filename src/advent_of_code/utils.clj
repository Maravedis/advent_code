(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]))

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

(defn count-if [pred coll]
  (count (keep #(when (pred %) %) coll)))


(def char->digit {\1 1
                  \2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9
                  \0 0})

(def digit-map {"one"   1
                "two"   2
                "three" 3
                "four"  4
                "five"  5
                "six"   6
                "seven" 7
                "eight" 8
                "nine"  9})

(defn sum [coll]
  (apply + coll))

(defn nums [string]
  (map read-string (re-seq #"\d+" string)))

(defn fix [f] (fn g [& args] (apply f g args))) ; fix inline memoization, thanks stack overflow

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn tprint [x]
  (pprint x)
  x)

(defn get-input 
  ([year day] (get-input year day (slurp ".SESSION_ID")))
  ([year day session-id]
   (->> (http/get (str "https://adventofcode.com/" year "/day/" day "/input")
                  {:headers {:cookie (str "session=" session-id)}})
        :body
        (spit (str "inputs/" year "/" day ".in")))
   (str year "/" day ".in")))

 