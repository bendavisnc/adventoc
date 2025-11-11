(ns adventoc.ancillary.dijkstra.dijkstra-test
  (:require
   [adventoc.ancillary.dijkstra.dijkstra :as core]
   [clojure.test :refer :all]))

(def graph {:d {:a 4 :e 2}
            :a {:c 3 :e 4}
            :e {:c 4 :g 5}
            :c {:f 5 :b 2}
            :b {:f 2}
            :g {:f 5}})

(deftest dijkstra-test
  (testing "dijkstra"
    ;; (time (core/print-distances graph :d))
    (is (= [4 8 6 0 2 10 7]
           (vals (core/dijkstra graph :d))))))
