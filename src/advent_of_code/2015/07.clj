(ns advent-of-code.2015.07
  (:require [advent-of-code.utils :as u]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-instruction [line]
  (let [[h i j k] (str/split line #" (-> )?")]
    (cond
      (= (type (read-string h)) java.lang.Long) {:var   (keyword i)
                                                 :value (read-string h)}
      (= h "NOT") {:op     bit-not
                   :l  (keyword i)
                   :r nil
                   :o (keyword j)}
      :else {:op     (case i
                       "AND" bit-and
                       "OR" bit-or
                       "LSHIFT" bit-shift-left
                       "RSHIFT" bit-shift-right)
             :l      (keyword h)
             :r      (case i
                       ("LSHIFT" "RSHIFT") (read-string j) 
                       (keyword j))
             :o (keyword k)})))

(defn parse-circuit [path]
  (u/read-file-list path read-instruction))

(defn run [path]
  (let [instructions (u/read-file-list path read-instruction)]
    (loop [[i & r] instructions
           results {}]
      (if i
        (if (contains? i :var)
          (recur r (assoc results (:var i) (:value i))) 
          (recur r (cond-> results
                     (keyword? (:r i)) (assoc (:o i) ((:op i) (get results (:l i)) (get results (:r i))))
                     (number? (:r i)) (assoc (:o i) ((:op i) ((:l i) results) (:r i)))
                     (nil? (:r i)) (assoc (:o i) ((:op i) ((:l i) results))))))
        (bit-and 0xFFFF (:a results))
        #_(->> results
             (map (fn [[k v]] [k (bit-and 0xFFFF v)]))
             (into {}))))))

(comment
  (def path (u/get-input 2015 7))
  (def test-path "2015/day7_test")
  (type (read-string "NOT"))            (println "got here ")

  (parse-circuit path)

  (run test-path)
  (run path)
  )