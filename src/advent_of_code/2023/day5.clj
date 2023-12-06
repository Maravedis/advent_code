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
           ss                  (apply sorted-map (if part2? seeds (interleave seeds (repeat 1))))
           next-step           (sorted-map)]
      (if-let [[[d sx r] & ms] sh]
        (if-let [[ax l] (first ss)]
          (let [ay (+ ax l)
                sy (+ sx r)]
            (cond
              (and (> ax sy) ms) (recur (cons ms st) ss next-step)
              (> ax sy)  (recur st (apply conj next-step ss) (sorted-map))
              (<= ay sy) (recur steps (dissoc ss ax) (assoc next-step (+ d (- ax sx)) l))
              :else (recur (cons ms st) (assoc (dissoc ss ax) sy (- ay sy)) (assoc next-step (+ d (- ax sx)) (- sy ax)))))
          (recur st next-step (sorted-map)))
        (ffirst ss)))))

(comment 
  (gen-solve "2023/5.in" false) ; ~705µs
  (gen-solve "2023/5.in" true)  ; ~1.0ms
  )