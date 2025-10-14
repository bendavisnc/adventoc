(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]])
  (:gen-class))

(defprotocol Amphipod
  :extend-via-metadata true
  (energy-cost [this])
  (amphipod-name [this]))

(def A (with-meta `A
         {:energy-cost (fn [_] 1)
          :amphipod-name (fn [_] "Amber")}))

(def B (with-meta `B
         {:energy-cost (fn [_] 10)
          :amphipod-name (fn [_] "Bronze")}))

(def C (with-meta `C
         {:energy-cost (fn [_] 100)
          :amphipod-name (fn [_] "Copper")}))

(def D (with-meta `D
         {:energy-cost (fn [_] 1000)
          :amphipod-name (fn [_] "Desert")}))

(defmethod print-method clojure.lang.Symbol [sym ^java.io.Writer w]
  (if (-> sym meta :amphipod-name)
    (.write w (name sym))
    (.write w (str sym))))

(def burrow {:hallway (vec (repeat 11 nil))
             :room {:A [nil, nil]
                    :B [nil, nil]
                    :C [nil, nil]
                    :D [nil, nil]}
             :indexes {:A 2
                       :B 4
                       :C 6
                       :D 8}})

(defn -main [& _]
  (let [burrow-state (-> burrow
                         (assoc-in [:room :A 0] A)
                         (assoc-in [:room :A 1] B)
                         (assoc-in [:room :B 0] D)
                         (assoc-in [:room :B 1] C)
                         (assoc-in [:room :C 0] C)
                         (assoc-in [:room :C 1] B)
                         (assoc-in [:room :D 0] A)
                         (assoc-in [:room :D 1] D))]
    (println burrow-state)))
