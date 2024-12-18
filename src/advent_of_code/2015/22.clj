(ns advent-of-code.2015.22
  (:require
   [advent-of-code.utils :as u]))

(def player {:hp      50
             :defense 0
             :mana    500
             :effects {:shield   0
                       :recharge 0
                       :poison   0}})

(def player-hard (assoc player :hard true))

(def boss {:hp      55
           :damage  8
           :effects {:poison   0
                     :recharge 0
                     :shield   0}})

(def spells [{:cost   53
              :damage 4}
             {:cost   73
              :damage 2
              :heal   2}
             {:cost   113
              :shield 6}
             {:cost   173
              :poison 6}
             {:cost     229
              :recharge 5}])

(defn handle-effects [{:keys [effects]
                       :as   player}]
  (let [{:keys [shield recharge poison]} effects]
    (cond-> player
      (= 1 shield) (assoc :defense 0)
      (<= 1 recharge) (update :mana + 101)
      (<= 1 poison) (update :hp - 3)
      :always (-> (update-in [:effects :shield] u/dec0)
                  (update-in [:effects :recharge] u/dec0)
                  (update-in [:effects :poison] u/dec0)))))

(defn cast-spell [player boss mana-expended {:keys [cost damage heal shield poison recharge]} turn]
  (when (or (>= (:mana player) cost)
            (and recharge (= 0 (get-in [:effects :recharge] player)))
            (and shield (= 0 (get-in [:effects :shield] player)))
            (and poison (= 0 (get-in [:effects :poison] boss))))
    [(cond-> player
       heal (update :hp + heal)
       recharge (assoc-in [:effects :recharge] recharge)
       shield (-> (assoc :defense 7)
                  (assoc-in [:effects :shield] shield))
       :always (update :mana - cost))
     (cond-> boss
       damage (update :hp - damage)
       poison (assoc-in [:effects :poison] poison))
     (+ mana-expended cost)
     (inc turn)]))

(def run-game
  (memoize
   (fn [player boss mana-expended turn]
     (let [player (cond-> (handle-effects player)
                    (and (even? turn) (:hard player)) (update :hp - 1))
           boss   (handle-effects boss)]
       (cond  (<= (:hp player) 0)   Integer/MAX_VALUE
              (<= (:hp boss) 0)     mana-expended
              (< (:mana player) 53) Integer/MAX_VALUE
              (> turn 20)           Integer/MAX_VALUE ; cut at 20 to have a faster time
              (even? turn)        (->> (keep #(cast-spell player boss mana-expended % turn) spells)
                                       (map #(apply run-game %))
                                       (reduce min Integer/MAX_VALUE))
              (odd? turn)         (run-game (update player :hp - (max 1 (- (:damage boss) (:defense player))))
                                            boss
                                            mana-expended
                                            (inc turn)))))))
(comment
  (def path (u/get-input 2015 22))
  (def tpath "2024/22_test.in")

  (run-game player boss 0 0)
  (run-game player-hard boss 0 0))