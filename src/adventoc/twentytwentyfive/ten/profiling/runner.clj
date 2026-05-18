(ns adventoc.twentytwentyfive.ten.profiling.runner
  (:require
    [adventoc.twentytwentyfive.ten.factory :as core]
    [adventoc.twentytwentyfive.ten.machine :as m]))

(defn run-solver-tiny
  []
  (core/-main))
  ;; (core/machine->least-button-presses
  ;;  ;; (m/str->machine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")))
  ;;  (m/str->machine
  ;;   "[...#..] (2,3,4,5) (2,3) (0,1,3,4) (1,2,5) (5) (0,1)
  ;;   {23,42,42,29,15,33}")))

