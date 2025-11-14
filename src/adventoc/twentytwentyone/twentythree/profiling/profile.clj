(ns adventoc.twentytwentyone.twentythree.profiling.profile
  (:require
   [adventoc.twentytwentyone.twentythree.profiling.runner :as runner]
   [clj-async-profiler.core :as prof]))

(defn profile-tiny! []
  (prof/profile
    (runner/run-solver-tiny))
  (prof/serve-ui 8080))

(comment
  (profile-tiny!))

;; Run code you want to profile here
