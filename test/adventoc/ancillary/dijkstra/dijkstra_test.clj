(ns adventoc.ancillary.dijkstra.dijkstra-test
  (:require
   [adventoc.ancillary.dijkstra.dijkstra :as core]
   [clojure.test :refer :all]))

(def vertex-data [:a :b :c :d :e :f :g])

(def adj-matrix
  (-> (vec (repeat (count vertex-data) (vec (repeat (count vertex-data) 0))))
      (assoc-in [3 0] 4)
      (assoc-in [3 4] 2)
      (assoc-in [0 2] 3)
      (assoc-in [0 4] 4)
      (assoc-in [4 2] 4)
      (assoc-in [4 6] 5)
      (assoc-in [2 5] 5)
      (assoc-in [2 1] 2)
      (assoc-in [1 5] 2)
      (assoc-in [6 5] 5)))

(def graph {:adj-matrix adj-matrix
            :vertex-data vertex-data})

(deftest dijkstra-test
  (testing "dijkstra"
    (is (= [4 8 6 0 2 10 7]
           (core/dijkstra graph :d)))))
