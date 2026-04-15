(ns adventoc.twentytwentyfive.ten.factory-test
  (:require
    [adventoc.twentytwentyfive.ten.factory :as core]
    [clojure.string :as string]
    [clojure.test :refer :all]))

(def machine
  (core/str->machine "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"))

(deftest lights->str-test
  (testing "lights->str"
    (is (= (core/lights->str [false true true false])
           "[.##.]"))))

(deftest buttons-test
  (testing "buttons"
    (is (= (::core/buttons machine)
           [[3] [1 3] [2] [2 3] [0 2] [0 1]]))))

(deftest push-button-test
  (testing "push button"
    (are [button expected] (is (= (core/lights->str
                                   (::core/lights (core/push-button machine
                                                                    button)))
                                  expected))
     (get-in machine [::core/buttons 0]) "[...#]"
     (get-in machine [::core/buttons 1]) "[.#.#]"
     (get-in machine [::core/buttons 2]) "[..#.]"
     (get-in machine [::core/buttons 3]) "[..##]"
     (get-in machine [::core/buttons 4]) "[#.#.]"
     (get-in machine [::core/buttons 5]) "[##..]")))


(deftest push-buttons-test
  (testing "push buttons"
    (are [buttons] (is (= (core/lights->str (::core/lights
                                             (core/push-buttons machine
                                                                buttons)))
                          "[.##.]"))
     [(get-in machine [::core/buttons 0])
      (get-in machine [::core/buttons 1])
      (get-in machine [::core/buttons 2])]

     [(get-in machine [::core/buttons 1])
      (get-in machine [::core/buttons 3])
      (get-in machine [::core/buttons 5])
      (get-in machine [::core/buttons 5])]

     [(get-in machine [::core/buttons 0])
      (get-in machine [::core/buttons 2])
      (get-in machine [::core/buttons 3])
      (get-in machine [::core/buttons 4])
      (get-in machine [::core/buttons 5])]

     [(get-in machine [::core/buttons 4])
      (get-in machine [::core/buttons 5])])))


(deftest find-least-buttons-solution-test
  (testing "find-least-buttons-solution"
    (is (= (:button-indexes (core/find-least-buttons-solution machine))
           [1 3]))))
