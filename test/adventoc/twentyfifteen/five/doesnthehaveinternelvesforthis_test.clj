(ns adventoc.twentyfifteen.five.doesnthehaveinternelvesforthis-test
  (:require
   [adventoc.twentyfifteen.five.doesnthehaveinternelvesforthis :as core]
   [clojure.test :refer :all]))

(deftest is-nice-test
  (testing "is-nice?"
    (is (= true (core/is-nice? "ugknbfddgicrmopn")))))

(deftest is-nice-less-ridiculous-test
  (testing "is-nice-less-ridiculous?"
    (is (= true (core/is-nice-less-ridiculous? "qjhvhtzxzqqjkmpb")))
    (is (= true (core/is-nice-less-ridiculous? "xxyxx")))
    (is (= false (core/is-nice-less-ridiculous? "uurcxstgmygtbstg")))
    (is (= false (core/is-nice-less-ridiculous? "ieodomkazucvgmuy")))
    (is (= true (core/is-nice-less-ridiculous? "xyxy")))
    (is (= true (core/is-nice-less-ridiculous? "aabcdefgaaxyx")))
    (is (= false (core/is-nice-less-ridiculous? "aaa")))))
