(ns advent-of-code.2015.11
  (:require [advent-of-code.utils :as u]))

(defn ->number [password] (mapv #(- (int %) 97) password))
(defn ->password [number] (apply str (map #(char (+ % 97)) number)))

(defn doubles? [pass]
  (loop [[a & [b :as t]] pass
         fi false
         se false]
    (cond (and fi se) true
          (not b)     false     
          fi          (recur t fi (and (not= fi a) (= a b) a)) 
          :else       (recur t (and (= a b) a) se))))

(defn check [pass]
  (and (not-any? #(or (= 8 %) (= 11 %) (= 14 %)) pass)
       (some (fn [[a b c]] (= (+ 2 a) (+ 1 b) c)) (partition 3 1 pass))
       (doubles? pass)))

(defn inc-pass [pass]
  (let [h (peek pass)]
    (if (= h 25)
      (conj (inc-pass (pop pass)) 0)
      (conj (pop pass) (case h 7 (+ 2 h) 10 (+ 2 h) 13 (+ 2 h) (inc h))))))

(defn part1 [password]
  (loop [pass (->number password)]
    (if (check pass)
      (->password pass)
      (recur (inc-pass pass)))))

(defn part2 [password]
   (loop [pass (->number password)
          sw   false] 
     (let [c (check pass)]
       (if (and sw c)
         (->password pass)
         (recur (inc-pass pass) (or sw c))))))

(comment
  (def path (u/get-input 2015 11)) 
  (part1 (first (u/read-file-list path identity)))
  (part2 (first (u/read-file-list path identity)))
  )
