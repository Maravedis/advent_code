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

(defn run [instructions k]
  (let [ec-fn        (fn [dfs a]
                       (let [curr   (instructions a)
                             arg-fn (fn [x] (if (or (nil? x) (int? x)) x (dfs x)))]
                         (cond (map? curr) (bit-and 0xFFFF ((:op curr) (arg-fn (curr :l)) (arg-fn (curr :r))))
                               (keyword? curr) (dfs curr)
                               :else curr)))
        ec-m         (u/fix (memoize ec-fn))]
    (ec-m k)))

(defn part1 [path]
  (run (parse-circuit path) :a))

(defn part2 [path]
  (let [instructions (parse-circuit path)]
    (run (assoc instructions :b (run instructions :a)) :a)))

(comment 
  (part1 "2015/7.in")
  (part2 "2015/7.in")
  )