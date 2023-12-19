(ns advent-of-code.2023.19
  (:require [advent-of-code.utils :as u]))

(defn parse-rule [rule]
  (let [[_ name conds fail] (re-find #"(\w+)\{(.*),(\w+)" rule)
        rules                 (->> (re-seq #"([xmas])([<>])(\d+):(\w+)" conds)
                                   (map (fn [[_ car comp num dest]] 
                                          (fn [piece] 
                                            (when ((case comp ">" > "<" <) (piece car) (parse-long num)) dest)))))]
    [name {:conds rules
           :fail  fail}]))

(defn parse-piece [piece]
  (let [values (re-seq #"([xmas])=(\d+)" piece)]
    (->> values
         (map (fn [[_ k v]] [k (parse-long v)]))
         (into {}))))

(defn part1 [path]
  (let [[rules pieces] (u/read-file-segmented-list path identity)
        rules (->> (map parse-rule rules)
                   (into {}))
        pieces (map parse-piece pieces)]
    (loop [[h & t :as xs] pieces
           curr    "in"
           accepted (transient [])]
      (if h
        (let [{:keys [conds fail]} (rules curr)
              v (or (first (keep #(% h) conds)) fail)]
          (case v
            "A" (recur t "in" (conj! accepted h))
            "R" (recur t "in" accepted)
            (recur xs v accepted)))
        (reduce (fn [res p] (apply + res (vals p))) 0 (persistent! accepted))))))


(defn part2 [path]
  (let [[rules _] (u/read-file-segmented-list path identity)
        rules          (->> (map parse-rule rules)
                            (into {}))
        pieces (for [x (range 1 4001)
                     m (range 1 4001)
                     a (range 1 4001)
                     s (range 1 4001)] {"x" x "m" m "a" a "s" s})]
    (loop [[h & t :as xs] pieces
           curr           "in"
           accepted       (transient [])]
      (if h
        (let [{:keys [conds fail]} (rules curr)
              v                    (or (first (keep #(% h) conds)) fail)]
          (case v
            "A" (recur t "in" (conj! accepted h))
            "R" (recur t "in" accepted)
            (recur xs v accepted)))
        (count accepted)))))

(comment
  (def path (u/get-input 2023 19))
  (def tpath "2023/19_test.in")

  (parse-rule "px{a<2006:qkq,m>2090:A,rfg}")
  (parse-piece "{x=787,m=2655,a=1222,s=2876}")

  (part1 path)
  (* 4000 4000 4000 4000)
  (part2 tpath)
  )