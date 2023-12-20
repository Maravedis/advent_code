(ns advent-of-code.2023.20
  (:require [advent-of-code.utils :as u]
            [clojure.math.numeric-tower :refer [lcm]]
            [clojure.string :refer [split]]))

(defn parse-line [line]
  (let [[module dest] (split line #" -> ")
        [_ type name] (re-find #"([%&])?(\w+)" module)
        dests         (split dest #", ")]
    [(keyword name) {:type  (case type "%" :ff "&" :conj :broadcast)
                     :dests (map keyword dests)}]))

(defn connect-inputs [modules]
  (->> (map (fn [[k {:keys [type] :as v}]]
              (case type
                :ff [k (assoc v :state false)]
                :conj [k (assoc v :state (->> (for [[in {:keys [dests]}] modules
                                                    :when                (some #(= k %) dests)]
                                                [in false])
                                              (into {})))]
                [k v])) modules)
       (into {})))

(defn broadcast [queue pulse dests origin]
  (reduce (fn [q dest] (conj q [pulse dest origin])) (pop queue) dests))

(defn gen-solve [modules acc update-acc-fn end-fn result-fn] 
  (loop [pulses  (conj clojure.lang.PersistentQueue/EMPTY [false :broadcaster :button])
         modules (connect-inputs modules)
         acc     acc
         pushes  1]
    (if-let [[pulse dest orig] (peek pulses)]
      (let [{:keys [dests state type]} (modules dest)]
        (case type
          :broadcast (recur (broadcast pulses pulse dests dest)
                            modules
                            (update-acc-fn acc pulse orig pushes)
                            pushes)
          :ff (if (not pulse)
                (recur (broadcast pulses (not state) dests dest)
                       (update-in modules [dest :state] not)
                       (update-acc-fn acc pulse orig pushes)
                       pushes)
                (recur (pop pulses) modules (update-acc-fn acc pulse orig pushes) pushes))
          :conj (let [inputs    (assoc state orig pulse)
                      new-pulse (not (reduce #(and %1 %2) (vals inputs)))]
                  (recur (broadcast pulses new-pulse dests dest)
                         (assoc-in modules [dest :state] inputs)
                         (update-acc-fn acc pulse orig pushes)
                         pushes))
          (recur (pop pulses) modules (update-acc-fn acc pulse orig pushes) pushes)))
      (if (end-fn acc pushes)
        (result-fn acc)
        (recur (conj pulses [false :broadcaster :button]) modules acc (inc pushes))))))

(defn part1 [path]
  (gen-solve (into {} (u/read-file-list path parse-line))
             [0 0]
             (fn [[high low] pulse & _] (if pulse [(inc high) low] [high (inc low)]))
             (fn [_ pushes] (= pushes 1000))
             (fn [[high low]] (* high low))))

; Analyzing the input, we can see that the pulses are reversed before the last conj into rx.
; So we find the combinators before those reversing, find their period on low pulses, then take the lcm.
(defn part2 [path]
  (let [modules     (into {} (u/read-file-list path parse-line))
        rx-orig     (first (keep (fn [[k {:keys [dests]}]] (when (some #(= :rx %) dests) k)) modules))
        dn-origs    (set (keep (fn [[k {:keys [dests]}]] (when (some #(= rx-orig %) dests) k)) modules))
        combinators (keep (fn [[k {:keys [dests]}]] (when (some #(dn-origs %) dests) k)) modules)]
    (gen-solve modules
               (zipmap combinators (repeat nil))
               (fn [acc pulse orig pushes] (if (and (not pulse) (contains? acc orig)) (assoc acc orig pushes) acc))
               (fn [acc _] (every? some? (vals acc)))
               (fn [acc] (reduce #(lcm %1 %2) (vals acc))))))

(comment 
  (def path (u/get-input 2023 20))
  (def tpath "2023/20_test.in")

  (part1 path)
  (part2 path)
  )