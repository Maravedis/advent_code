(ns advent-of-code.2024.21
  (:require [advent-of-code.utils :as u]
            [clojure.set :refer [map-invert]]
            [clojure.string :as str]))

(def path (u/get-input 2024 21))
(def tpath (u/test-path 2024 21))

(def keypad {[0 0] 7
             [0 1] 8
             [0 2] 9
             [1 0] 4
             [1 1] 5
             [1 2] 6
             [2 0] 1
             [2 1] 2
             [2 2] 3
             [3 1] 0
             [3 2] \A})

(def dapyek (map-invert keypad))

(def dpad {[0 1] \^
           [0 2] \A
           [1 0] \<
           [1 1] \v
           [1 2] \>})

(def dapd (map-invert dpad))

(defn find-sequence-keypad [digit [rs cs]]
  (let [[re ce]    (dapyek digit)
        r-diff     (abs (- re rs))
        c-diff     (abs (- ce cs))
        vertical   (if (< re rs) (repeat r-diff \^) (repeat r-diff \v))
        horizontal (if (> ce cs) (repeat c-diff \>) (repeat c-diff \<))]
    (cond-> #{}
      (not (and (= ce 0) (= rs 3))) (conj (concat horizontal vertical [\A]))
      (not (and (= re 3) (= cs 0))) (conj (concat vertical horizontal [\A])))))

(defn find-sequence-dpad [dir [rs cs]]
  (let [[re ce]    (dapd dir)
        r-diff     (abs (- re rs))
        c-diff     (abs (- ce cs))
        horizontal (if (> ce cs) (repeat c-diff \>) (repeat c-diff \<))
        vertical   (if (> re rs) (repeat r-diff \v) (repeat r-diff \^))]
    (cond
      (empty? vertical) [(concat horizontal [\A])]
      (empty? horizontal) [(concat vertical [\A])]
      (and (= rs 0) (= ce 0)) [(concat vertical horizontal [\A])]
      (and (= cs 0) (= re 0)) [(concat horizontal vertical [\A])]
      :else [(concat vertical horizontal [\A]) (concat horizontal vertical [\A])])))

(defn enter-line-keypad [line]
  (->> (reduce (fn [[curr acc] h]
                 [(dapyek h)
                  (mapcat (fn [nm] (map #(concat % nm) acc)) (find-sequence-keypad h curr))])
               [[3 2] #{[]}] line)
       second))

(defn enter-line-dpap [line]
  (->> (reduce (fn [[curr acc] h]
                 [(dapd h)
                  (mapcat (fn [nm] (map #(concat % nm) acc)) (find-sequence-dpad h curr))])
               [[0 2] #{[]}] line)
       second))

(defn back-to-A [chunk last-char]
  (case last-char
    \^ [(concat chunk [\>])]
    \> [(concat chunk [\^])]
    \v [(concat chunk [\^ \>]) (concat chunk [\> \^])]
    \< [(concat chunk [\> \> \^])]))

(def to-next-line
  (memoize
   (fn [depth line]
     (if (= 0 depth)
       (count line)
       (let [chunks (partition-by #{\A} line)]
         (->> (map-indexed (fn [idx chunk]
                             (if (odd? idx) (count chunk)
                                 (->> chunk
                                      (enter-line-dpap)
                                      (mapcat #(back-to-A % (last chunk)))
                                      (map #(to-next-line (dec depth) %))
                                      (apply min)))) chunks)
              (reduce +)))))))

(defn solve [path depth]
  (let [input (->> (u/read-file-list path vec)
                   (map drop-last)
                   (map #(map u/char->digit %))
                   (map #(concat % [\A])))]
    (->> input
         (map (fn [line]
                (let [number   (parse-long (str/join (drop-last line)))
                      sequence (->> line
                                    (enter-line-keypad)
                                    (map #(to-next-line depth %))
                                    (apply min))]
                  [sequence number])))
         (reduce #(+ %1 (apply * %2)) 0))))

(comment

  ;329431019997766
  (solve path 2)
  (solve path 25))
