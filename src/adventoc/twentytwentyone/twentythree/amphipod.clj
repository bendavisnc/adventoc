(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]])
  (:gen-class))

(defprotocol Amphipod
  :extend-via-metadata true
  (energy-cost [this])
  (amphipod-name [this]))

(def A (with-meta `A
         {`energy-cost (fn [_] 1)
          `amphipod-name (fn [_] "Amber")}))

(def B (with-meta `B
         {`energy-cost (fn [_] 10)
          `amphipod-name (fn [_] "Bronze")}))

(def C (with-meta `C
         {`energy-cost (fn [_] 100)
          `amphipod-name (fn [_] "Copper")}))

(def D (with-meta `D
         {`energy-cost (fn [_] 1000)
          `amphipod-name (fn [_] "Desert")}))

(defn -main [& _]
  (dorun
    (for [amphipod [A B C D]]
      (do
        (println (energy-cost amphipod))
        (println (amphipod-name amphipod))))))
