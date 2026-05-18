(ns adventoc.twentytwentyfive.ten.profiling.profile
  (:require
    [adventoc.twentytwentyfive.ten.profiling.runner :as runner]
    [clj-async-profiler.core :as prof]))

(defn profile-tiny!
  []
  (prof/profile
   (runner/run-solver-tiny))
  (prof/serve-ui 8080))

(comment
  (profile-tiny!))


;; sudo sysctl -w kernel.perf_event_paranoid=1
;; sudo sysctl -w kernel.kptr_restrict=0

