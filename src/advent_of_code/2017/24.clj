(ns advent-of-code.2017.24
  (:require [advent-of-code.utils :as u]))

(def path (u/get-input 2017 24))
(def tpath (u/test-path 2017 24))

(defn ->ports-map [components]
  (loop [res               (transient {})
         [[l r :as h] & t] components]
    (if (nil? h)
      (->> (persistent! res)
           (map (fn [[k v]] [k (frequencies v)]))
           (into {}))
      (recur (-> res
                 (u/update! l (fnil conj []) h)
                 (u/update! r (fnil conj []) h))
             t))))

(defn comp-bridge [[c1 s1 :as b1] [c2 s2 :as b2]]
  (cond
    (< c1 c2) b2
    (> c1 c2) b1
    (> s1 s2) b1
    :else b2))

(defn build-bridges
  ([components f] (build-bridges [] 0 f components))
  ([bridge end f components]
   (let [possibilities (get components end {})]
     (if (empty? possibilities)
       [(count bridge) (apply + (flatten bridge))]
       (f (map
           (fn [[[r l :as component] x]]
             (let [other-end (if (= r end) l r)]
               (build-bridges
                (conj bridge component)
                other-end
                f
                (if (= end other-end)
                  (update components end (if (= x 2) #(dissoc % component) #(- % 2)))
                  (let [y (get-in components [other-end component])]
                    (-> (update components end (if (= x 1) #(dissoc % component) dec))
                        (update other-end (if (= y 1) #(dissoc % component) dec))))))))
           possibilities))))))

(defn solve [path f]
  (let [input (->> (u/read-file-list path u/nums)
                   ->ports-map)]
    (build-bridges input f)))

(comment
  ; part1
  (solve tpath #(apply max-key second %))
  (solve path #(apply max-key second %))
  ; part2
  (solve tpath #(reduce comp-bridge %))
  (solve path #(reduce comp-bridge %)))
