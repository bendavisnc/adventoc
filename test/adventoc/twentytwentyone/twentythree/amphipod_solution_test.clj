(ns adventoc.twentytwentyone.twentythree.amphipod-solution-test
  (:require
   [adventoc.twentytwentyone.twentythree.amphipod :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(def journey-start-tiny {:accumulated-cost 0
                         :moves [[:A0 [:room :A 1]]
                                 [:B0 [:room :A 0]]
                                 [:B1 [:room :B 1]]
                                 [:A1 [:room :B 0]]]
                         :cost [0, 0, 0, 0]})

(def journey-start {:accumulated-cost 0
                    :moves [[:A0 [:room :A 0]]
                            [:B0 [:room :A 1]]
                            [:D0 [:room :B 0]]
                            [:C0 [:room :B 1]]
                            [:C1 [:room :C 0]]
                            [:B1 [:room :C 1]]
                            [:A1 [:room :D 0]]
                            [:D1 [:room :D 1]]]
                    :cost [0, 0, 0, 0, 0, 0, 0, 0]})

(deftest solution-tiny-test
  (testing "solution-tiny"
    (binding [core/rooms (take 2 core/rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (let [solution (time (core/amphipod-solve journey-start-tiny))]
        (println (core/journey->breadcrumbs solution))
        (is (= 112 (:accumulated-cost solution)))))
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)))

(deftest solution-test
  (testing "solution"
    (let [solution (time (core/amphipod-solve journey-start))]
      (println (core/journey->breadcrumbs solution))
      (is (= 12521 (:accumulated-cost solution))))))
