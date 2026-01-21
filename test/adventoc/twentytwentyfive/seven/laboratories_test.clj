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
