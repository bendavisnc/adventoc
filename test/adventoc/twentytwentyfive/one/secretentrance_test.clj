(ns adventoc.twentytwentyfive.one.secretentrance-test
  (:require
   [adventoc.twentytwentyfive.one.secretentrance :as core]
   [clojure.test :refer :all]))

(deftest dialstep->n-test
  (testing "dialstep->n returns correct values"
    (is (thrown? Exception (core/dialstep->n nil)))
    (is (= -1
           (core/dialstep->n "L1")))
    (is (= 1
           (core/dialstep->n "R1")))))

(deftest apply-dialsteps-test
  (testing "apply-dialsteps works as expected"
    (is (= 3
          (->>
            ["L68" "L30", "R48", "L5" "R60" "L55" "L1" "L99" "R14" "L82"]
            (map core/dialstep->n)
            (core/apply-dialsteps [50])
            (filter zero?)
            count)))))

