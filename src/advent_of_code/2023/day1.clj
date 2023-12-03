(ns advent-of-code.2023.day1
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(def reg (re-pattern (str "^" (str/join "|^" (keys u/digit-map)))))

(defn find-first-occurence [input parse-fn] 
  (loop [[h & t] input]
    (if-let [res (parse-fn h)]
      res
      (recur t))))

(defn ->digit [[h]] (u/char->digit h))
(defn ->number [l] (or (->digit l) (u/digit-map (re-find reg l))))

(defn first-and-last [f line]
  (let [l     (count line)
        first (find-first-occurence (for [i (range 0 l)] (subs line i l)) f) 
        last  (find-first-occurence (for [i (range (dec l) -1 -1)] (subs line i l)) f)] 
    (+ (* 10 first) last)))

(defn gen-solve [path f]
  (->> (u/read-file-list path (partial first-and-last f)) 
       (apply +)))

(comment
  (time (gen-solve "2023/1.in" ->digit))
  (time (gen-solve "2023/1.in" ->number))
  )