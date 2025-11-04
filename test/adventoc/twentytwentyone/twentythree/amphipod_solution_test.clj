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
                         :seen #{}})

(deftest solution-test
  (testing "solution"
    (binding [core/rooms (take 2 core/rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (let [solution (core/amphipod-solve journey-start-tiny)]
        (println (core/journey->breadcrumbs solution))
        (is (= 68 (:accumulated-cost solution)))))))
