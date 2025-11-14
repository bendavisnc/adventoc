(ns adventoc.twentytwentyone.twentythree.profiling.profile
  (:require
   [adventoc.twentytwentyone.twentythree.profiling.runner :as runner]
   [clj-async-profiler.core :as prof]))

(defn profile-tiny! []
  (prof/start {:event :cpu})
  (runner/run-solver-tiny)
  (prof/stop {:generate-flamegraph? false}))

(comment
  (profile-tiny!))

;; Run code you want to profile here
