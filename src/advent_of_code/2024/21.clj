(ns advent-of-code.2024.21
  (:require [advent-of-code.utils :as u]
            [advent-of-code.points :as p]
            [clojure.set :refer [map-invert]]
            [clojure.string :as str]))

(def path (u/get-input 2024 21))
(def tpath (u/test-path 2024 21))

(def keypad (->> [[7 8 9]
                  [4 5 6]
                  [1 2 3]
                  [:deadspot 0 \A]]
                 p/->grid
                 map-invert))

(def dpad (->> [[:deadspot \^ \A]
                [\< \v \>]]
               p/->grid
               map-invert))

(defn find-sequence-grid [rgrid target-value [rs cs]]
  (let [[re ce]     (rgrid target-value)
        [ds-r ds-c] (rgrid :deadspot)
        r-diff      (abs (- re rs))
        c-diff      (abs (- ce cs))
        horizontal  (if (> ce cs) (repeat c-diff \>) (repeat c-diff \<))
        vertical    (if (> re rs) (repeat r-diff \v) (repeat r-diff \^))]
    (cond-> []
      (not (and (= ce ds-c) (= rs ds-r))) (conj (concat horizontal vertical [\A]))
      (not (and (= re ds-r) (= cs ds-c))) (conj (concat vertical horizontal [\A])))))

(defn enter-line [rgrid line]
  (->> (reduce (fn [[curr acc] h]
                 [(rgrid h)
                  (mapcat (fn [nm] (map #(concat % nm) acc)) (find-sequence-grid rgrid h curr))])
               [(rgrid \A) #{[]}] line)
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
                                 (->> (enter-line dpad chunk)
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
                      sequence (->> (enter-line keypad line)
                                    (map #(to-next-line depth %))
                                    (apply min))]
                  [sequence number])))
         (reduce #(+ %1 (apply * %2)) 0))))

(comment

  (time (solve path 2))
  (time (solve path 25)))
