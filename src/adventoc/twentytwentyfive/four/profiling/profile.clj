(ns adventoc.twentytwentyfive.four.profiling.profile
  (:require
   [adventoc.twentytwentyfive.four.profiling.runner :as runner]
   [clj-async-profiler.core :as prof]))

(defn profile-tiny! []
  (prof/profile
    (runner/run-solver-tiny))
  (prof/serve-ui 8080))

(comment
  (profile-tiny!))
