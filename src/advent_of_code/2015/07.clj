(ns advent-of-code.2015.07
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn read-val [x] (if-let [val (re-find #"\d+" x)] (parse-long val) (keyword x)))

(defn read-instruction [line]
  (let [[h i j k] (str/split line #" (-> )?")
        bit-not-2 (fn [x _] (bit-not x))]
    (cond
      (not (or j k)) [(keyword i) (read-val h)]
      (not k)        [(keyword j) {:op bit-not-2
                                   :l  (read-val i)}]
      :else          [(keyword k) {:op (case i
                                         "AND" bit-and
                                         "OR" bit-or
                                         "RSHIFT" unsigned-bit-shift-right
                                         "LSHIFT" bit-shift-left)
                                   :l  (read-val h)
                                   :r  (read-val j)}])))

(defn parse-circuit [path]
  (->> (u/read-file-list path read-instruction)
       (into {})))

(defn execute-circuit [instructions a]
  (println a)
  (let [curr (instructions a)
        arg-fn (fn [x] (if (or (nil? x) (int? x)) x (execute-circuit instructions x)))]
    (cond (map? curr) (bit-and 0xFFFF ((:op curr) (arg-fn (curr :l)) (arg-fn (curr :r))))
          (keyword? curr) (execute-circuit instructions curr)
          :else curr)))

(defn run [path k]
  (let [instructions (parse-circuit path)]
    (execute-circuit instructions k) )
  )
(comment

  (def path (u/get-input 2015 7))
  (def test-path "2015/day7_test")
  (run path :a)
  (filter #(nil? (:l (second %))) (parse-circuit path))
  )