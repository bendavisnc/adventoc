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
