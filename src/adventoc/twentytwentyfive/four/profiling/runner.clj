(ns adventoc.twentytwentyfive.four.profiling.runner
  (:require
   [adventoc.twentytwentyfive.four.printingdepartment :as core]))

(defn run-solver-tiny []
  (core/printingdepartment "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." {:continuous-removal? true}))
