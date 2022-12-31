(ns advent-of-code.2022.day16
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn read-valve [line]
  (let [s (str/split line #"([;,]? )|=")
        coor-fn (fn [i] (first (drop i s)))]
    [(coor-fn 1) {:rate  (read-string (coor-fn 5))
                  :conns (->> (drop 10 s)
                              (map (fn [k] [k 1]))
                              (into {}))}]))

(defn compress-graph [graph] 
  (loop [graph graph
         i     0]
    (if-let [[k {:keys [conns]}] (first (drop-while (fn [[k {:keys [rate]}]] (or (= k "AA") (not= rate 0))) graph))] 
      (recur (dissoc (->> (map (fn [[k' v]]
                                 (if (contains? (:conns v) k)
                                   [k' (-> (update v :conns
                                                   #(reduce (fn [acc1 [dest cost]]
                                                              (if (not= dest k')
                                                                (assoc acc1 dest (+ cost (get-in v [:conns k])))
                                                                acc1))
                                                            % conns))
                                           (update :conns #(dissoc % k)))]
                                   [k' v]))
                               graph)
                          (into {}))
                     k)
             (inc i))
      graph)))

(defn distances [graph]
  (let [nodes  (sort (keys graph))
        dist-i (->> (map (fn [i]
                           [i (->> (map (fn [j] [j (get-in graph [i :conns j] 1000)]) nodes)
                                   (into {}))])
                         nodes)
                    (map (fn [[k v]] [k (assoc v k 0)]))
                    (into {}))]
    (loop [[hi & ti :as i] nodes
           [hj & tj :as j] nodes
           [hk & tk :as k] nodes
           dist            dist-i]
      (cond (nil? hi) dist
            (nil? hj) (recur ti nodes nodes dist)
            (nil? hk) (recur i tj nodes dist)
            :else (recur i j tk
                         (update-in dist
                                    [hi hj]
                                    #(min % (+ (get-in dist [hi hk]) (get-in dist [hk hj])))))))))

(defn run [path part2?]
  (let [input (->> (u/read-file-list path read-valve)
                   (into {})
                   (compress-graph))
        time  (if part2? 26 30)
        dist  (distances input)
        dfs   (fn [dfs valve minutes opened first?]
                (->> input
                     (filter (fn [[node {:keys [rate]}]] (and (not (contains? opened node))
                                                              (not= 0 rate)
                                                              (< 0 (get-in dist [valve node]) minutes))))
                     (map (fn [[node {:keys [rate]}]]
                            (let [d         (inc (get-in dist [valve node]))
                                  time-left (- minutes d)]
                              (+ (* time-left rate) (dfs node time-left (conj opened valve) first?)))))
                     (reduce max (if first? (dfs "AA" time (conj opened valve) false) 0)))) 
        dfs-m (u/fix (memoize dfs))]
    (dfs-m "AA" time #{"AA"} part2?)))
  
(comment
  (time (run "2022/day16" false))
  (time (run "2022/day16" true))
  )