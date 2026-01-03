(ns adventoc.twentytwentyfive.four.printingdepartment-test
  (:require
   [adventoc.twentytwentyfive.four.printingdepartment :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(deftest occupied?-test
  (testing "occupied?"
    (let [graph (core/input->graph (string/join "\n"
                                                ["@.@"
                                                 "@@@"]))]
      (is (core/occupied? graph [0 0]))
      (is (not (core/occupied? graph [1, 0]))))))

(deftest adjacencies-test
  (testing "adjacencies"
    (is (= [[0 1] [1 0] [1 1]]
           (core/adjacencies [0, 0] [10, 10])))
    (is (= [[1 0] [1 1] [2 1] [3 0] [3 1]]
           (core/adjacencies [2, 0] [10, 10])))))

(deftest increment-adjacents-test
  (testing "increment-adjacents"
    (is (= {1 {0 1, 1 1}, 0 {1 1}}
           (core/increment-adjacents {} [0 0] [10, 10])))))

(deftest counts-no-more-than-test
  (testing "counts-no-more-than"
    (let [graph (core/input->graph (string/join "\n"
                                                ["..@@.@@@@."
                                                 "@@@.@.@.@@"
                                                 "@@@@@.@.@@"
                                                 "@.@@@@..@."
                                                 "@@.@@@@.@@"
                                                 ".@@@@@@@.@"
                                                 ".@.@.@.@@@"
                                                 "@.@@@.@@@@"
                                                 ".@@@@@@@@."
                                                 "@.@.@@@.@."]))
          counts (core/adjacency-counts graph)]
      (is (= 13 (core/counts-no-more-than graph counts 4))))))
