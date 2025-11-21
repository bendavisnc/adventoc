(ns adventoc.twentyfifteen.three.perfectlysphericalhousesinavacuum-test
  (:require
   [adventoc.twentyfifteen.three.perfectlysphericalhousesinavacuum :as core]
   [clojure.test :refer :all]))

(deftest arrow->coord-test
  (testing "arrow->coord returns correct values"
    (is (thrown? Exception (core/arrow->coord \a)))
    (is (= [[0 1] [1 0] [0 -1] [-1 0]] (map core/arrow->coord "^>v<")))))

(deftest perfectlysphericalhousesinavacuum-test
  (testing "> delivers presents to 2 houses: one at the starting location, and one to the east."
    (is (= 2
           (count (core/perfectlysphericalhousesinavacuum-solve ">")))))
  (testing "^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south."
    (is (= 3
           (core/perfectlysphericalhousesinavacuum "^v" {:robo-santa? true}))))
  (testing "^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location."
    (is (= 2
           (get (core/perfectlysphericalhousesinavacuum-solve "^>v<")
                [0, 0])))
    (is (= 3
           (core/perfectlysphericalhousesinavacuum "^>v<" {:robo-santa? true}))))
  (testing "^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses."
    (is (= 2
          (count (core/perfectlysphericalhousesinavacuum-solve "^v^v^v^v^v"))))
    (is (= 11
           (core/perfectlysphericalhousesinavacuum  "^v^v^v^v^v" {:robo-santa? true})))))
