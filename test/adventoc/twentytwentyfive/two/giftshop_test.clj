(ns adventoc.twentytwentyfive.two.giftshop-test
  (:require
   [adventoc.twentytwentyfive.two.giftshop :as core]
   [clojure.test :refer :all]))

(deftest is-valid?-test
  (testing "is-valid?"
    (is (= true
           (core/is-valid? "1")))
    (is (= false
           (core/is-valid? "55")))

    (is (= false
           (core/is-valid? "6464")))
    (is (= true
           (core/is-valid? "64642")))))

(deftest invalid-ids-test
  (testing "invalid ids are correctly identified"
    (let [input->invalid-ids #(filter (comp not core/is-valid?) (core/input->ids %))]
      (is (= ["38593859"]
             (input->invalid-ids "38593856-38593862")))
      (is (= []
             (input->invalid-ids "2121212118-2121212124"))))))
