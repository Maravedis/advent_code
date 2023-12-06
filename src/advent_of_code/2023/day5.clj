(ns advent-of-code.2023.day5
  (:require [advent-of-code.utils :as u]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- pad [[h :as lines]]
  (if (not= 0 (second h)) (cons [0 0 (second h)] lines) lines))

(defn read-almanach [path]
  (let [[seeds & maps] (str/split (slurp (io/resource path)) #"\n\n")]
    [(u/nums seeds) (map #(->> (str/split-lines %) (drop 1) (map u/nums) (sort-by second) pad) maps)]))

(defn gen-solve [path part2?]
  (let [[seeds maps] (read-almanach path)]
    (loop [[sh & st :as steps] maps
           [h & t :as ss]      (apply sorted-set (if part2? (map vec (partition 2 seeds)) (map (fn [seed] [seed 1]) seeds)))
           next-step           (sorted-set)]
      (if-let [[[d sx r] & ms] sh]
        (if-let [[ax l] h]
          (let [ay (+ ax l)
                sy (+ sx r)]
            (cond
              (and (> ax sy) ms) (recur (cons ms st) ss next-step)
              (> ax sy)  (recur st (apply conj next-step ss) (sorted-set))
              (<= ay sy) (recur steps t (conj next-step [(+ d (- ax sx)) l]))
              :else (recur (cons ms st) (conj t [sy (- ay sy)]) (conj next-step [(+ d (- ax sx)) (- sy ax)]))))
          (recur st next-step (sorted-set)))
        (apply min (map first ss))))))

(comment
  (gen-solve "2023/5.in" false) ; ~740µs
  (gen-solve "2023/5.in" true)  ; ~1.1ms
  )