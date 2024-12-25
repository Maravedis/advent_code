(ns advent-of-code.2016.10
  (:require [advent-of-code.utils :refer [conjset] :as u]
            [clojure.core.match :refer [match]]))

(def path (u/get-input 2016 10))
(def tpath (u/test-path 2016 10))

(defn parse-line [line]
  (-> (re-seq #"\d+|bot|output" line)
      vec
      (match
       [val "bot" bot]                        [:init (parse-long bot) (parse-long val)]
       ["bot" bot "bot" low "bot" high]       [(parse-long bot) [0 (parse-long low) (parse-long high)]]
       ["bot" bot "output" low "bot" high]    [(parse-long bot) [1 (parse-long low) (parse-long high)]]
       ["bot" bot "bot" low "output" high]    [(parse-long bot) [2 (parse-long low) (parse-long high)]]
       ["bot" bot "output" low "output" high] [(parse-long bot) [3 (parse-long low) (parse-long high)]])))

(defn parse-input [path]
  (let [{init true
         ins  false} (->> (u/read-file-list path parse-line)
                          (group-by #(= :init (first %))))
        instructions           (into {} ins)
        init                   (->> (map #(drop 1 %) init)
                                    (reduce #(update %1 (first %2) conjset (second %2)) {}))
        start                  (some (fn [[k v]] (and (= 2 (count v)) k)) init)]
    [init instructions start]))

(defn run [instructions state open]
  (reduce (fn [acc bot]
            (let [values        (state bot)
                  [op low high] (instructions bot)]
              (case op
                0 (-> (update acc low conjset (apply min values))
                      (update high conjset (apply max values)))
                1 (-> (update-in acc [:output low] conjset (apply min values))
                      (update high conjset (apply max values)))
                2 (-> (update acc low conjset (apply min values))
                      (update-in [:output high] conjset (apply max values)))
                3  (-> (update-in acc [:output low] conjset (apply min values))
                       (update-in [:output high] conjset (apply max values)))))) state open))

(defn part1 [path]
  (let [[init instructions start] (parse-input path)]
    (loop [open  [start]
           state init]
      (if-let [k (some (fn [[k chips]] (and (= chips #{61 17}) k)) state)]
        k
        (let [new-state (run instructions state open)]
          (recur (keep (fn [[k v]] (when (and (not= k :output) (> (count v) 1)) k)) new-state) new-state))))))

(defn part2 [path]
  (let [[init instructions start] (parse-input path)]
    (loop [open  [start]
           state init]
      (if (and (get-in state [:output 0]) (get-in state [:output 1]) (get-in state [:output 2]))
        (* (first (get-in state [:output 0])) (first (get-in state [:output 1])) (first (get-in state [:output 2])))
        (let [new-state (run instructions state open)]
          (recur (keep (fn [[k v]] (when (and (not= k :output) (> (count v) 1)) k)) new-state) new-state))))))

(comment
  (part1 path)
  (part2 path)
  ;
  )
