(ns adventoc.ancillary.dijkstra.dijkstra-test
  (:require
    [adventoc.ancillary.dijkstra.dijkstra :as core]
    [clojure.test :refer :all]
    [orchestra.spec.test :as st]
    [clojure.spec.alpha :as spec]))

(spec/def ::core/node keyword?)
;; (spec/def ::core/node boolean?)

(spec/def ::core/start ::core/node)

(spec/def ::core/node-distances (spec/map-of ::core/node int?))

(spec/fdef core/dijkstra
 :args
 (spec/cat :graph (spec/map-of ::core/start ::core/node-distances)
           :start keyword?)
 :ret ::core/node-distances)

(spec/fdef core/dijkstra-q
 :args
 (spec/cat :graph (spec/map-of ::core/start ::core/node-distances)
           :start keyword?)
 :ret ::core/node-distances)


(st/instrument)

(def graph
  {:d {:a 4 :e 2}
   :a {:c 3 :e 4}
   :e {:c 4 :g 5}
   :c {:f 5 :b 2}
   :b {:f 2}
   :g {:f 5}})

(deftest dijkstra-test
  (are [dijkstra]
   (is (= (dijkstra graph :d)
          {:a 4 :b 8 :c 6 :d 0 :e 2 :f 10 :g 7}))
   core/dijkstra
   core/dijkstra-q))