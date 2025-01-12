(ns advent-of-code.2017.08
  (:require [advent-of-code.utils :as u]
            [clojure.string :refer [split]]))

(def path (u/get-input 2017 8))
(def tpath (u/test-path 2017 8))

(defn resolve-f [s]
  (fn [x y]
    (case s
      "inc" (+ (or x 0) y)
      "dec" (- (or x 0) y))))

(defn resolve-chck [s]
  (case s
    "!=" not=
    (resolve (symbol s))))

(defn parse-line [line]
  (let [[target f operand1 _ chck op operand2] (split line #" ")]
    [(fn [registers] (update registers (keyword target) (resolve-f f) (parse-long operand1)))
     (fn [registers] ((resolve-chck op) (get registers (keyword chck) 0) (parse-long operand2)))]))

(defn run [path]
  (let [input (->> (u/read-file-list path parse-line))]
    (-> (reduce (fn [[registers curr-max :as acc] [f g]]
                  (if (g registers)
                    (let [regs (f registers)]
                      [regs (apply max curr-max (map second regs))])
                    acc)) [{} Integer/MIN_VALUE] input)
        (update 0 #(apply max (map second %))))))

(comment
  (run path)
  ;
  )
