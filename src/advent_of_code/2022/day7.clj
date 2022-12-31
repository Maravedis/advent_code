(ns advent-of-code.2022.day7
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]))

(defn parse-fs [instructions]
  (loop [[[h s t] & r] instructions
         stack         []
         acc           {}]
    (if (nil? h)
      acc
      (case h
        "$" (case s
              "cd" (case t
                     "/" (recur r stack acc)
                     ".." (recur r (pop stack) acc)
                     (recur r (conj stack t) acc))
              "ls" (recur r stack acc))
        "dir" (recur r stack acc)
        (recur r stack (update-in acc (conj stack :files) concat [(read-string h)]))))))

(defn calc-node-value [fs]
  (let [leafs (->> (map (fn [[k v]] [k (calc-node-value v)]) (dissoc fs :files))
                   (into {}))]
    (-> (assoc fs :value (apply + (apply + (or (:files fs)
                                               [0]))
                                (or (seq (map :value (vals leafs)))
                                    [0])))
        (dissoc :files)
        (merge leafs))))

(defn sum-at-most-100000 [{:keys [value] :as fs}]
  (if (nil? value)
    0
    (apply + 
           (or (when (< value 100000) value) 0)
           (map sum-at-most-100000 (vals (dissoc fs :value :files))))))

(defn run-first [resource]
  (->> (u/read-file-list resource #(str/split % #" "))
       (parse-fs)
       (calc-node-value) 
       (sum-at-most-100000)))

(defn find-smallest-necessary [at-least acc {:keys [value]
                                             :as   fs}]
  (if (nil? fs)
    acc
    (let [v (if (<= at-least value acc) value acc)]
      (apply min v (map #(find-smallest-necessary at-least v %) (vals (dissoc fs :value)))))))

(defn run-second [resource]
  (let [fs           (->> (u/read-file-list resource #(str/split % #" "))
                          (parse-fs)
                          (calc-node-value))
        space-needed (- 30000000 (- 70000000 (:value fs)))]
    (find-smallest-necessary space-needed (Integer/MAX_VALUE) fs)))
 

(comment
  (run-first "2022/day7")
  (run-second "2022/day7")
  )
