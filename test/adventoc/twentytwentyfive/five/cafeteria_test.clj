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
