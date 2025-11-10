(ns adventoc.ancillary.dijkstra.dijkstra-test
  (:require
   [adventoc.ancillary.dijkstra.dijkstra :as core]
   [clojure.test :refer :all]))

(def vertex-data [:a :b :c :d :e :f :g])

(def adj-matrix {:d {:a 4 :e 2}
                 :a {:c 3 :e 4}
                 :e {:c 4 :g 5}
                 :c {:f 5 :b 2}
                 :b {:f 2}
                 :g {:f 5}})

(def graph {:adj-matrix adj-matrix
            :vertex-data vertex-data})

(deftest dijkstra-test
  (testing "dijkstra"
    (is (= [4 8 6 0 2 10 7]
           (vals (core/dijkstra graph :d))))
    (core/print-distances graph :d)))
