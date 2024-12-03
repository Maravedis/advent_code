(ns advent-of-code.2015.19
  (:require
   [advent-of-code.utils :as u]
   [clojure.core.rrb-vector :as fv]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [com.rpl.specter :as sp]))

(defn ->tokens [s]
  (vec (re-seq #"[A-Z][a-z]*" s)))

(defn transform-one [ins]
  (memoize
   (fn [tokens]
     (let [n      (count tokens)]
       (loop [i   0
              acc #{}]
         (if (= i n)
           acc
           (recur (inc i)
                  (apply conj acc (for [t (get ins (get tokens i))]
                                    (fv/catvec (fv/subvec tokens 0 i)
                                               (fv/vec t)
                                               (fv/subvec tokens (inc i) n)))))))))))

(defn part1 [path]
  (let [[ins input] (-> (slurp (io/resource path))
                        (str/split #"\n\n"))
        ins         (->> (str/split-lines ins)
                         (map #(str/split % #" => "))
                         u/group-by-key
                         (sp/transform [sp/MAP-VALS sp/ALL] ->tokens))
        input       (->> (str/split-lines input)
                         first
                         ->tokens)
        next-state  (transform-one ins)]
    (count (disj (next-state input) input))))

; Shamelessly stolen from here : https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju/
(defn part2 [path]
  (let [[_ input] (-> (slurp (io/resource path))
                      (str/split #"\n\n"))
        input     (->> (str/split-lines input)
                       first
                       ->tokens)
        n         (count input)
        rnar      (+ (u/count-when #(= "Rn" %) input)
                     (u/count-when #(= "Ar" %) input))
        y         (u/count-when #(= "Y" %) input)]
    (- n rnar (* 2 y) 1)))

(comment
  (def path (u/get-input 2015 19))
  (def tpath "2015/19_test.in")

  (part1 tpath)
  (part2 tpath)

  (part1 path)
  (part2 path))