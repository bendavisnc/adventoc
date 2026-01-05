(ns adventoc.twentytwentyfive.four.printingdepartment-test
  (:require
   [adventoc.twentytwentyfive.four.printingdepartment :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(deftest occupied?-test
  (testing "occupied?"
    (let [grid (core/input->grid (string/join "\n"
                                   ["@.@"
                                    "@@@"]))]
      (is (core/occupied? grid [0 0]))
      (is (not (core/occupied? grid [1, 0]))))))

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
    (let [grid (core/input->grid (string/join "\n"
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
          counts (core/adjacency-counts grid)]
      (is (= 13 (core/counts-less-than grid counts core/adjacency-limit))))))

(deftest grid-next-test
  (testing "grid-next"
    (let [grid (core/input->grid (string/join "\n"
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
          counts (core/adjacency-counts grid)
          grid-next (core/grid-next grid counts core/adjacency-limit)
          counts-next (core/adjacency-counts grid-next)
          _ (is (= 12 (core/counts-less-than grid-next counts-next core/adjacency-limit)))
          grid-next-next (core/grid-next grid-next counts-next core/adjacency-limit)
          counts-next-next (core/adjacency-counts grid-next-next)
          _ (is (= 7 (core/counts-less-than grid-next-next counts-next-next core/adjacency-limit)))]
      nil)))
