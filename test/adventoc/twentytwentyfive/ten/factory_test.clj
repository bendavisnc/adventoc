(ns adventoc.twentytwentyfive.ten.factory-test
  (:require
    [adventoc.twentytwentyfive.ten.factory :as core]
    [adventoc.twentytwentyfive.ten.machine :refer [■ 💡] :as m]
    [adventoc.twentytwentyfive.ten.specs.machine]
    [adventoc.twentytwentyfive.ten.specs.factory]
    [clojure.test :refer :all]
    [orchestra.spec.test :as st]))

(st/instrument)

(deftest find-least-buttons-lights-solution-test
  (testing "find-least-buttons-lights-solution"
    (are [machine expected]
     (is (= (core/machine->least-button-presses-lights machine)
            expected))
     (m/str->machine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}") 2
     (m/str->machine
      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}") 3
     (m/str->machine
      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}") 2
     (m/str->machine
      "[...#..] (2,3,4,5) (2,3) (0,1,3,4) (1,2,5) (5) (0,1) {23,42,42,29,15,33}")
      5
     (m/str->machine
      "[...#.#] (2,5) (0,1) (0,3,4) (3,5) (4) (1,4) (0,1,2,3) (0,1,2,5)
      {43,48,41,28,198,42}")
      1)))


(deftest find-least-buttons-joltage-solution-test
  (testing "find-least-buttons-joltage-solution"
    (are [machine expected]
     (is (= (core/machine->least-button-presses-joltages machine)
            expected))
     (m/str->machine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}") 10
     (m/str->machine
      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}") 12
     (m/str->machine
      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}") 11
     (m/str->machine
      "[...#..] (2,3,4,5) (2,3) (0,1,3,4) (1,2,5) (5) (0,1) {23,42,42,29,15,33}")
      70
     (m/str->machine
      "[...#.#] (2,5) (0,1) (0,3,4) (3,5) (4) (1,4) (0,1,2,3) (0,1,2,5)
      {43,48,41,28,198,42}")
      240)))
