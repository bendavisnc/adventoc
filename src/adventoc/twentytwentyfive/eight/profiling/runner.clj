(ns adventoc.twentytwentyfive.eight.profiling.runner
  (:require
    [adventoc.helpers :refer [input]]
    [adventoc.twentytwentyfive.eight.playground :as core]))

(defn run-solver
  []
  (core/playground (input "adventoc/twentytwentyfive/eight/input.txt")
                   {:connect-all? true}))