(ns advent-of-code.2023.day5
  (:require [advent-of-code.utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- pad [[h :as lines]]
  (if (not= 0 (second h)) (cons [0 0 (second h)] lines) lines))

(defn read-almanach [path]
  (let [[seeds & maps] (str/split (slurp (io/resource path)) #"\n\n")]
    [(u/nums seeds) (map #(->> (str/split-lines %) (drop 1) (map u/nums) (sort-by second) pad) maps)]))

(defn next-step [[ax l] [[d sx r :as m] & ms]]
  (if (nil? m)
    [[ax l]]
    (let [ay (+ ax l)
          sy (+ sx r)]
      (cond
        (and (< ax sx) (<= sx ay sy)) [[d (- ay sx)]]
        (<= ax sx sy ay)              (conj (next-step [sy (- ay sy)] ms) [d r])
        (and (< sx ax) (<= ax sy ay)) (conj (next-step [sy (- ay sy)] ms) [(+ d (- ax sx)) (- sy ax)])
        (<= sx ax ay sy)              [[(+ d (- ax sx)) l]]
        :else (next-step [ax l] ms)))))

(defn gen-solve [path part2?]
  (let [[seeds maps] (read-almanach path)]
    (loop [[sh & st :as steps] maps
           [h & t :as ss]      (if part2? (partition 2 seeds) (map (fn [seed] [seed 1]) seeds))
           result              []]
      (if (nil? sh)
        (apply min (map first ss))
        (if (nil? h)
          (recur st result [])
          (recur steps t (concat (next-step h sh) result)))))))

(comment 
  (gen-solve "2023/5.in" false) ; ~1.7ms
  (gen-solve "2023/5.in" true)  ; ~3ms
  )