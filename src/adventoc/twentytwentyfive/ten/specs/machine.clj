(ns adventoc.twentytwentyfive.ten.specs.machine
  (:require
    [adventoc.twentytwentyfive.ten.machine :as m]
    [clojure.spec.alpha :as spec]))

(spec/def ::m/button (spec/coll-of integer?))

(spec/def ::m/button-indexes (spec/coll-of integer?))

(spec/def ::m/buttons (spec/coll-of ::m/button))

(spec/def ::m/joltage integer?)

(spec/def ::m/joltages (spec/coll-of ::m/joltage))

(spec/def ::m/joltages-diagram ::m/joltages)

(spec/def ::m/light boolean?)

(spec/def ::m/lights (spec/coll-of ::m/light))

(spec/def ::m/lights-diagram ::m/lights)

(spec/def ::m/machine
  (spec/keys :req
             [::m/lights ::m/lights-diagram ::m/buttons ::m/joltages
              ::m/joltages-diagram]))

(spec/fdef m/str->machine
 :args
 (spec/cat :s
           string?)
 :ret ::m/machine)
