(ns advent-of-code.2016.11
  (:require [advent-of-code.points :as p]
            [advent-of-code.utils :as u]
            [clojure.math.combinatorics :refer [subsets]]
            [clojure.set :refer [subset? union]]))

(def path (u/get-input 2016 11))
(def tpath (u/test-path 2016 11))

(def empty-floor {:generators #{}
                  :microchips #{}})

(defn parse-input [path]
  (let [lines (u/read-file-list path identity)]
    (->> (map #(re-seq #"(\w+)(?:-compatible)? (microchip|generator)" %) lines)
         (map-indexed vector)
         (map (fn [[floor items]] [floor (reduce (fn [acc [_ atom item]] (update acc (keyword (str item "s")) conj (keyword atom))) empty-floor items)]))
         (into {}))))

(defn valid-state? [{:keys [floors]}]
  (every? (fn [[_ {:keys [generators microchips]}]]
            (or (subset? microchips generators)
                (empty? generators))) floors))

(defn move-elevator [{:keys [floors elevator]}]
  (let [{:keys [generators microchips]} (floors elevator)
        new-elevators                   (cond-> []
                                          (< elevator 3) (conj (inc elevator))
                                          (> elevator 0) (conj (dec elevator)))]
    (for [new-elevator new-elevators
          generator    (take-while #(<= (count %) 2) (subsets (vec generators)))
          microchip    (take-while #(<= (count %) 2) (subsets (vec microchips)))
          :when        (and (< 0 (+ (count generator) (count microchip)) 3)
                            (not (and (= 2 (+ (count generator) (count microchip)))
                                      (> elevator new-elevator))))]
      {:floors   (-> floors
                     (update-in [elevator :generators] #(apply disj % generator))
                     (update-in [elevator :microchips] #(apply disj % microchip))
                     (update-in [new-elevator :generators] #(apply conj % generator))
                     (update-in [new-elevator :microchips] #(apply conj % microchip)))
       :elevator new-elevator})))

(defn make-end-state [init-state]
  (-> (reduce (fn [acc floor]
                (-> (update-in acc [:floors 3 :generators] union (get-in acc [:floors floor :generators]))
                    (update-in [:floors 3 :microchips] union (get-in acc [:floors floor :microchips]))
                    (assoc-in [:floors floor :generators] #{})
                    (assoc-in [:floors floor :microchips] #{}))) init-state [0 1 2])
      (assoc :elevator 3)))

(defn solve [path part2?]
  (let [init-state   (cond-> {:floors   (parse-input path)
                              :elevator 0}
                       part2? (update-in [:floors 0 :generators] union #{:elerium :dilithium})
                       part2? (update-in [:floors 0 :microchips] union #{:elerium :dilithium}))
        end-state    (make-end-state init-state)
        heuristic-fn (fn [{:keys [floors elevator]} _]
                       (reduce-kv (fn [acc floor {:keys [generators microchips]}]
                                    (+ acc
                                       (if (or (not-empty generators) (not-empty microchips))
                                         (* 3 (+ (abs (- floor elevator)) (* (- 3 floor) (+ 1 (count generators) (count microchips)))))
                                         0)))
                                  0 floors))]
    (p/a-star-score init-state end-state #(->> (move-elevator %) (filter valid-state?)) heuristic-fn)))

(comment

  (time (solve path false)) ; 28ms. Stumbled onto a good heuristic?
  (time (solve path true)) ; 53s. I seriously do not know what I'm doing
  )
