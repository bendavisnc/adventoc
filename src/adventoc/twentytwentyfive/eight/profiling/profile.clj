(ns adventoc.twentytwentyfive.eight.profiling.profile
  (:require
    [adventoc.twentytwentyfive.eight.profiling.runner :as runner]
    [clj-async-profiler.core :as prof]))

(defn profile!
  []
  (prof/profile
   (runner/run-solver))
  (prof/serve-ui 8080))

(comment
  (profile!))
