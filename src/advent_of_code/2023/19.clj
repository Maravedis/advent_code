(ns advent-of-code.2023.19
  (:require [advent-of-code.utils :as u]))

(def var-idx {"x" 0
              "m" 1
              "a" 2
              "s" 3})

(defn parse-rule [rule]
  (let [[_ name conds fail] (re-find #"(\w+)\{(.*),(\w+)" rule)
        conds (re-seq #"([xmas])([<>])(\d+):(\w+)" conds)
        rules (map (fn [[_ car comp num dest]]
                     (fn [piece]
                       (when ((case comp ">" > "<" <) (piece car) (parse-long num)) dest))) conds)]
    [name {:conds    rules
           :explicit (map (fn [[_ car comp num dest]]
                            [(var-idx car) comp (parse-long num) dest]) conds)
           :fail     fail}]))


(defn parse-piece [piece]
  (let [values (re-seq #"([xmas])=(\d+)" piece)]
    (->> values
         (map (fn [[_ k v]] [k (parse-long v)]))
         (into {}))))

(defn calc-combinations [[x-min x-max m-min m-max a-min a-max s-min s-max]]
  (* (- x-max x-min) (- m-max m-min) (- a-max a-min) (- s-max s-min)))

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

; ranges [x-min x-max m-min m-max a-min a-max s-min s-max]
(defn part2 
  ([path]
   (let [[rules _] (u/read-file-segmented-list path identity)
         rules     (->> (map parse-rule rules)
                        (into {}))]
     (part2 rules "in" [1 4001 1 4001 1 4001 1 4001])))
  ([rules curr ranges]
   (cond (= "A" curr) (calc-combinations ranges)
         (= "R" curr) 0
         :else
         (let [wf           (-> (get rules curr) :explicit)
               fail         (-> (get rules curr) :fail)
               [failed res] (reduce (fn [[r res] [car comp num dest]]
                                      (case comp
                                        ">" [(assoc r (inc (* 2 car)) (inc num))
                                             (+ res (part2 rules dest (assoc r (* car 2) (inc num))))]
                                        "<" [(assoc r (* 2 car) num)
                                             (+ res (part2 rules dest (assoc r (inc (* car 2)) num)))]))
                                    [ranges 0] wf)]
           (+ res (part2 rules fail failed))))))

(comment
  (def path (u/get-input 2023 19))
  (def tpath "2023/19_test.in")

  (part1 path)
  (part2 path)
  )