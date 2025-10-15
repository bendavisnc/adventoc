(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]])
  (:gen-class))

(def amphipod-energys {:A 1
                       :B 10
                       :C 100
                       :D 1000})

(def amphipods {::A0 {:moves []}
                ::A1 {:moves []}
                ::B0 {:moves []}
                ::B1 {:moves []}
                ::C0 {:moves []}
                ::C1 {:moves []}
                ::D0 {:moves []}
                ::D1 {:moves []}})

(defn amphipod-energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword amphipod-energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn -main [& _]
  (println (amphipod-energy ::C1)))

(comment (amphipod-energys (keyword (str (first (name ::B1))))))

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########
