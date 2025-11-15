(ns adventoc.twentyfifteen.two.iwastoldtherewouldbenomath-test
  (:require
   [adventoc.twentyfifteen.two.iwastoldtherewouldbenomath :as core]
   [clojure.test :refer :all]))

(deftest iwastoldtherewouldbenomath-test
  (testing "returns box surface area and smallest side surface area sum"
    (is (= 58 (core/iwastoldtherewouldbenomath 2 3 4)))
    (is (= 43 (core/iwastoldtherewouldbenomath 1 1 10)))))

(deftest feet-of-ribbon-test
  (testing "returns smallest perimeter and volume sum"
    (is (= 34 (core/feet-of-ribbon 2 3 4)))
    (is (= 14 (core/feet-of-ribbon 1 1 10)))))
