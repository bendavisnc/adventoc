(ns adventoc.twentytwentyfive.eight.playground-test
  (:require [adventoc.twentytwentyfive.eight.playground :as core]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(deftest distance-test
  (testing "distance" (is (= 14.0 (core/distance [6 4 -3] [2 -8 3])))))

(deftest coord-combos-test
  (testing "coord-combos"
    (let [choose-2-n (fn [n] (/ (* n (dec n)) 2))]
      (is (= [] (core/all-coord-combos 1)))
      (is (= [[1 0]] (core/all-coord-combos 2)))
      (is (= [[1 0] [2 0] [2 1]] (core/all-coord-combos 3)))
      (is (= [[1 0] [2 0] [2 1] [3 0] [3 1] [3 2]] (core/all-coord-combos 4)))
      (is (= (choose-2-n 5) (count (core/all-coord-combos 5))))
      (is (= (choose-2-n 10) (count (core/all-coord-combos 10))))
      (is (= (choose-2-n 20) (count (core/all-coord-combos 20)))))))

(deftest connections-test
  (testing "connections"
    (is
     (= {0  '(7 19)
         7  '(19 0)
         13 '(2)
         17 '(18)
         3  '(19)
         12 '(9)
         2  '(18 8 13)
         19 '(3 14 7 0)
         11 '(16)
         9  '(12)
         14 '(19)
         16 '(11)
         18 '(2 17)
         8  '(2)}
        (core/connections
         (core/coord-distances
          [[162 817 812] [57 618 57] [906 360 560] [592 479 940] [352 342 300]
           [466 668 158] [542 29 236] [431 825 988] [739 650 466] [52 470 668]
           [216 146 977] [819 987 18] [117 168 530] [805 96 715] [346 949 466]
           [970 615 88] [941 993 340] [862 61 35] [984 92 344] [425 690 689]]
          (core/all-coord-combos 20))
         10)))))

(deftest connection-groups-test
  (testing "connection-groups"
    (is
     (= [#{13 17 2 18 8} #{0 7 19 14} #{12 9} #{11 16}]
        (sort-by
         (comp #(* -1 %) count)
         (core/connection-groups
          {0  [7 19]
           7  [19 0]
           13 [2]
           17 [18]
           12 [9]
           2  [18 8 13]
           19 [14 7 0]
           11 [16]
           9  [12]
           14 [19]
           16 [11]
           18 [2 17]
           8  [2]}))))))

(deftest connect-all-test
  (testing "connect-all"
    (is
     (= [[[117 168 530]
          [216 146 977]]
         #{0 7 1 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}]
        (core/connect-all
         [[162 817 812] [57 618 57] [906 360 560] [592 479 940] [352 342 300]
          [466 668 158] [542 29 236] [431 825 988] [739 650 466] [52 470 668]
          [216 146 977] [819 987 18] [117 168 530] [805 96 715] [346 949 466]
          [970 615 88] [941 993 340] [862 61 35] [984 92 344]
          [425 690 689]])))))