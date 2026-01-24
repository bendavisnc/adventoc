(ns adventoc.twentytwentyfive.seven.laboratories-test
  (:require
   [adventoc.twentytwentyfive.seven.laboratories :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(deftest insert-beams-test
  (testing "insert-beams"
    (is (= ["...S..."
            "...|..."
            "..|^|.."
            "..|.|.."
            ".|^|^|."
            ".|.|.|."
            ".|.|.|."]
           (core/insert-beams  ["...S..."
                                "......."
                                "...^..."
                                "......."
                                "..^.^.."
                                "......."
                                "......."])))))

(deftest count-splinters-test
  (testing "beam-splinters-count"
    (is (= 3
           (core/beam-splinters-count (core/insert-beams  ["...S..."
                                                           "......."
                                                           "...^..."
                                                           "......."
                                                           "..^.^.."
                                                           "......."
                                                           "......."]))))))

(deftest splinter-possibilities-test
  (testing "splinter-possibilities"
    (is (=  {1 1, 3 2, 5 1}
            (core/splintering-exploration ["...S..."
                                           "......."
                                           "...^..."
                                           "......."
                                           "..^.^.."
                                           "......."
                                           "......."])))))
