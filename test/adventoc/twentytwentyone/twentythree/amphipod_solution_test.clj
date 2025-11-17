(ns adventoc.twentytwentyone.twentythree.amphipod-solution-test
  (:require
   [adventoc.twentytwentyone.twentythree.amphipod :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(def puzzle-tiny (string/join "\n" ["#############"
                                    "#...........#"
                                    "###B#A#.#.###"
                                    "  #A#B#.#.#"
                                    "  #########"]))

(def puzzle (string/join "\n" ["#############"
                               "#...........#"
                               "###B#C#B#D###"
                               "  #A#D#C#A#"
                               "  #########"]))

(deftest solution-tiny-test
  (testing "solution-tiny"
    (binding [core/rooms (take 2 core/rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (let [cost (time (core/amphipod puzzle-tiny))]
        (is (= 46 cost))))
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)))

(deftest solution-test
  (testing "solution"
    (let [cost (time (core/amphipod puzzle))]
      (is (= 12521 cost)))))
