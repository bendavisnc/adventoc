(ns adventoc.twentytwentyone.twentythree.profiling.runner
  (:require
   [adventoc.twentytwentyone.twentythree.amphipod :as core]))

(defn run-solver-tiny []
  (binding [core/rooms (take 2 core/rooms)]
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
    (core/amphipod-solve
      {:accumulated-cost 0
       :moves [[:A0 [:room :A 1]]
               [:B0 [:room :A 0]]
               [:B1 [:room :B 1]]
               [:A1 [:room :B 0]]]
       :cost [0 0 0 0]})))
