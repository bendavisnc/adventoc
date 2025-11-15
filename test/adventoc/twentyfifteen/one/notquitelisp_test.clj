(ns adventoc.twentyfifteen.one.notquitelisp-test
  (:require
   [adventoc.twentyfifteen.one.notquitelisp :as core]
   [clojure.test :refer :all]))

(deftest paren->num-test
  (testing "paren->num returns correct values"
    (is (thrown? Exception (core/paren->num \a)))
    (is (= 1 (core/paren->num \()))
    (is (= -1 (core/paren->num \))))))

(deftest notquitelisp-test
  (testing "(()) and ()() both result in floor 0."
    (is (= 0 (core/notquitelisp "(())")))
    (is (= 0 (core/notquitelisp "()()"))))
  (testing "((( and (()(()( both result in floor 3."
    (is (= 3 (core/notquitelisp "(((")))
    (is (= 3 (core/notquitelisp "(()(()("))))
  (testing "))((((( also results in floor 3."
    (is (= 3 (core/notquitelisp "))((((("))))
  (testing "()) and ))( both result in floor -1 (the first basement level)."
    (is (= -1 (core/notquitelisp "())")))
    (is (= -1 (core/notquitelisp "))("))))
  (testing "))) and )())()) both result in floor -3."
    (is (= -3 (core/notquitelisp ")))")))
    (is (= -3 (core/notquitelisp ")())())")))))

(deftest basement-position-test
  (testing ") causes him to enter the basement at character position 1"
    (is (= 1 (core/basement-position ")"))))
  (testing "()()) causes him to enter the basement at character position 5"
    (is (= 5 (core/basement-position "()())")))))
