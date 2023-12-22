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
          (mapv str/split-lines)
          (mapv #(mapv modifier-fn %))))))

(defn count-if [pred coll]
  (count (keep #(when (pred %) %) coll)))


(def char->digit {\1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \0 0})

(def digit-map {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 "zero" 0
                "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9 "0" 0})

(defn sum [coll]
  (apply + coll))

(defn dec0 [x] (max 0 (dec x)))

(defn between [coll pred]
  (->> coll (drop-while (complement pred)) (take-while pred)))

(defn nums [string]
  (map parse-long (re-seq #"-?\d+" string)))

(defn manhattan [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn re-pos
  "Return a list of pairs of (index, string) for all matches of `re` in `s`"
  [re s]
  (loop [m   (re-matcher re s)
         res (transient [])]
    (if (.find m)
      (recur m (conj! res [(.start m) (.group m)]))
      (rseq (persistent! res)))))

(defn fix [f] (fn g [& args] (apply f g args))) ; fix inline memoization, thanks stack overflow

(defn shoelace 
  "Given a list of vertices in the [x y] format representing a polygon, calculate the area of the polygon.
   The list must be of contiguous points. The first and last vertices must be equal for a loop to be formed."
  [points] 
  (/ (abs (reduce (fn [res [[a b] [c d]]] (+ res (- (* a d) (* b c)))) 0 (partition 2 1 points))) 2))

(defn area-vertices
  "Given a list of vertices in the [x y] format representing a polygon, calculate the number of vertices in the polygon (edge included).
    The list must be of contiguous points. The first and last vertices must be equal for a loop to be formed."
  [points]
  (+ 1
     (shoelace points)
     (/ (reduce #(+ %1 (apply manhattan %2)) 0 (partition 2 1 points)) 2)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn tee 
  ([x] (pprint x) x)
  ([file x] (with-open [w (io/writer file)] (pprint x w)) x))

(defn get-input 
  ([year day] (get-input year day (slurp ".SESSION_ID")))
  ([year day session-id]
   (let [filename (str "inputs/" year "/" day ".in")
         file     (io/file filename)]
     (when (not (.exists file))
       (->> (http/get (str "https://adventofcode.com/" year "/day/" day "/input")
                      {:headers {:cookie (str "session=" session-id)}})
            :body
            (spit file)))
     (str year "/" day ".in"))))

;; Transient Functions

(defn update! [xs k f & args]
  (assoc! xs k (apply f (get xs k) args)))

 