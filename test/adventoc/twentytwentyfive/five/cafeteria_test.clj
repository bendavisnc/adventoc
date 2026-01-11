(ns adventoc.twentytwentyfive.five.cafeteria-test
  (:require
   [adventoc.twentytwentyfive.five.cafeteria :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(deftest parse-input-test
  (testing "parse-input"
    (is (= [[[1 3] [5 7]] [0 2 6]]
           (core/input->fresh-ingrediant-ranges-and-ids (string/join "\n" ["1-3"
                                                                           "5-7"
                                                                           ""
                                                                           "0"
                                                                           "2"
                                                                           "6"]))))))

(deftest fresh-ids-count-test
  (testing "fresh-ids-count"
    (is (= 3
           (core/fresh-ids-count
             [[3,5], [10,14], [16,20], [12,18]]
             [1, 5, 8, 11, 17, 32])))))

(deftest ranges-no-intersections-test
  (testing "ranges-no-intersections"
    (is (= [[3, 5], [10, 20]]
           (core/ranges-no-intersections [[3,5], [10,14], [12,18], [13, 14] [16,20]])))
    (is (= [[3, 5], [10, 20]]
           (core/ranges-no-intersections [[3,5], [10,14], [16,20], [12,18]])))))

(deftest fresh-ids-range-sum-test
  (testing "fresh-ids-range-sum"
    (is (= 14
           (core/fresh-ids-range-sum [[3,5], [10,14], [16,20], [12,18]])))

    (is (= 14
           (core/fresh-ids-range-sum [[3,5], [10,14], [12,18], [13, 14] [16,20]])))))
