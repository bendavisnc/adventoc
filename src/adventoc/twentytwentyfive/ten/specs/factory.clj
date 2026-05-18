(ns adventoc.twentytwentyfive.ten.specs.factory
  (:require
    [adventoc.twentytwentyfive.ten.factory :as core]
    [adventoc.twentytwentyfive.ten.machine :as m]
    [clojure.spec.alpha :as spec]))

(spec/def ::increments (spec/+ #{0 1}))

(spec/def ::increments-seq (spec/coll-of ::increments :min-count 1))

(spec/def ::joltage->cost (spec/map-of ::m/joltages integer?))

(spec/def ::joltages-parity (spec/+ #{0 1}))

(spec/def ::joltages->parity->joltage->cost
  (spec/map-of ::joltages-parity ::joltage->cost))

(spec/fdef core/costs-by-parity
 :args (spec/cat :increments-seq
                 ::increments-seq)
 :ret  ::joltages->parity->joltage->cost)

(spec/fdef core/least-button-presses-joltages
 :args (spec/cat :increments-seq   ::increments-seq
                 :joltages-diagram ::m/joltages-diagram)
 :ret  integer?)

(spec/fdef core/machine->least-button-presses-lights
 :args
 (spec/cat :machine
           ::m/machine)
 :ret integer?)

(spec/fdef core/machine->least-button-presses-joltages
 :args
 (spec/cat :machine
           ::m/machine)
 :ret integer?)
