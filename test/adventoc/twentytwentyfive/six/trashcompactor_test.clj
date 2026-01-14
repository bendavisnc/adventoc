(ns adventoc.twentytwentyfive.six.trashcompactor-test
  (:require
   [adventoc.twentytwentyfive.six.trashcompactor :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(deftest input-to-grid-test
  (testing "input->grid"
    (is (= [["1" "2" "3"] ["4" "5" "6"] ["7" "8" "9"] ["+" "*" "*"]]
           (core/input->grid (string/join "\n" ["1 2 3"
                                                "4 5 6"
                                                "7 8 9"
                                                "+ * *"]))))))

(deftest input-to-grid-whitespace-included-test
  (testing "input->grid-keep-whitespace"
    (is (= [[\1 \space \2 \space \3] [\4 \space \5 \space \6] [\7 \space \8 \space \9] [\+ \space \* \space \*]]
           (core/input->grid-keep-whitespace (string/join "\n" ["1 2 3"
                                                                "4 5 6"
                                                                "7 8 9"
                                                                "+ * *"]))))))

(deftest process-math-ops-test
  (testing "process-math-ops"
    (let [process-math-ops (comp core/process-math-ops core/grid-rows-columns-switch core/input->grid)]
      (is (= [33210 490 4243455 401]
             (process-math-ops (string/join "\n" ["123 328  51 64 "
                                                  " 45 64  387 23 "
                                                  "  6 98  215 314 "
                                                  "*   +   *   + "])))))))

(deftest process-math-ops-right-to-left-test
  (testing "process-math-ops-right-to-left"
    (let [process-math-ops (comp core/process-math-ops-right->left core/grid-rows-columns-switch core/input->grid-keep-whitespace)]
      (is (= [8544 625 3253600 1058]
             (process-math-ops (string/join "\n" ["123 328  51 64 "
                                                  " 45 64  387 23 "
                                                  "  6 98  215 314 "
                                                  "*   +   *   + "])))))))
