(ns advent-of-code.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clj-http.client :as http]
            [com.rpl.specter :as sp]))

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
          (mapv str/split-lines)
          (mapv #(mapv modifier-fn %))))))

(def count-when (comp count filter))

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
                "nine"  9
                "zero"  0
                "1"     1
                "2"     2
                "3"     3
                "4"     4
                "5"     5
                "6"     6
                "7"     7
                "8"     8
                "9"     9
                "0"     0})

(defn sum [coll]
  (apply + coll))

(defn dec0 [x] (max 0 (dec x)))

(defn group-by-key [coll]
  (->> (group-by first coll)
       (sp/transform [sp/MAP-VALS] #(sp/select [sp/ALL (sp/nthpath 1)] %))))

(defn between [coll pred]
  (->> coll (drop-while (complement pred)) (take-while pred)))

(defn nums [string]
  (mapv parse-long (re-seq #"-?\d+" string)))

(defn index-max [coll]
  (first (reduce-kv (fn [[acc-i acc] idx value]
                      (if (> value acc)
                        [idx value]
                        [acc-i acc]))
                    [0 Integer/MIN_VALUE]
                    (vec coll))))

(defn queue
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([coll] (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(defn re-pos
  "Return a list of pairs of (index, string) for all matches of `re` in `s`"
  [re s]
  (loop [m   (re-matcher re s)
         res (transient [])]
    (if (.find m)
      (recur m (conj! res [(.start m) (.group m)]))
      (rseq (persistent! res)))))

(defn fix [f] (fn g [& args] (apply f g args))) ; fix inline memoization, thanks stack overflow

(defn tee
  ([x] (pprint x) x)
  ([file x] (with-open [w (io/writer file)] (pprint x w)) x))

(defn get-input
  ([year day] (get-input year day false (slurp ".SESSION_ID")))
  ([year day force?] (get-input year day force? (slurp ".SESSION_ID")))
  ([year day force? session-id]
   (let [filename (str "inputs/" year "/" day ".in")
         file     (io/file filename)]
     (when (or force? (not (.exists file)))
       (->> (http/get (str "https://adventofcode.com/" year "/day/" day "/input")
                      {:headers {:cookie (str "session=" session-id)}})
            :body
            (spit file)))
     (str year "/" day ".in"))))

;; Transient Functions

(defn update! [xs k f & args]
  (assoc! xs k (apply f (get xs k) args)))
