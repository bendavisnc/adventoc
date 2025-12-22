(ns adventoc.twentytwentyfive.three.lobby-test
  (:require
   [adventoc.twentytwentyfive.three.lobby :as core]
   [clojure.test :refer :all]))

(deftest batterybank->joltage-test
  (testing "batterybank->joltage"
    (is (= 98
           (core/batterybank->joltage "987654321111111")))
    (is (= 78
           (core/batterybank->joltage "234234234234278")))
    (is (= 89
           (core/batterybank->joltage "811111111111119")))
    (is (= 92
           (core/batterybank->joltage "818181911112111")))
    (is (= 99
           (core/batterybank->joltage "123912391238")))
    (is (= 99
           (core/batterybank->joltage "822144153333552393423468473433384235233463821334445547231435453323133344255983343614331222232859382")))))
