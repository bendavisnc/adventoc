(ns adventoc.twentytwentyfive.nine.movietheater-test
  (:require
    [adventoc.twentytwentyfive.nine.movietheater :as core]
    [clojure.string :as string]
    [clojure.test :refer :all]))


(deftest compressed-coords-test
  (testing "compressed-coords"
    (is (= [[1 1] [2 1]]
           (:coords (core/compressed-coords [[2 Integer/MAX_VALUE]
                                             [3 Integer/MAX_VALUE]]))))
    (let [{:keys [coords uncompress]}
          (core/compressed-coords [[7 1]
                                   [11 1]
                                   [11 7]
                                   [9 7]
                                   [9 5]
                                   [2 5]
                                   [2 3]
                                   [7 3]])]
      (is (= [[2 1] [4 1] [4 4] [3 4] [3 3] [1 3] [1 2] [2 2]]
             coords))
      (is (= [7 1]
             (uncompress [2 1])))
      (is (= [7 3]
             (uncompress [2 2]))))))

(deftest movetheater-test
  (let [input (string/join "\n"
                           (map #(string/join "," %)
                                [[7 1]
                                 [11 1]
                                 [11 7]
                                 [9 7]
                                 [9 5]
                                 [2 5]
                                 [2 3]
                                 [7 3]]))]
    (testing "movietheater"
      (let [result (core/movietheater input)]
        (is (= (string/join "\n"
                            ["⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
                             "⬜⬜🟪🟪🟪🟪🟪🟪🟪🟪🟪🟪⬜"
                             "⬜⬜🟪⬜⬜⬜⬜🟩⬛⬛⬛🟪⬜"
                             "⬜⬜🟪🟩🟩🟩🟩🟥⬛⬛⬛🟪⬜"
                             "⬜⬜🟪⬛⬛⬛⬛⬛⬛⬛⬛🟪⬜"
                             "⬜⬜🟪🟪🟪🟪🟪🟪🟪🟪🟪🟪⬜"
                             "⬜⬜⬜⬜⬜⬜⬜⬜⬜🟩⬛🟩⬜"
                             "⬜⬜⬜⬜⬜⬜⬜⬜⬜🟥🟩🟥⬜"
                             "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"])
               (:grid result)))
        (is (= 50
               (:area result)))))

    (testing "movietheater, respected boundary"
      (let [result (core/movietheater input {:respect-boundary? true})]
        (is (= (string/join "\n"
                            ["⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"
                             "⬜⬜⬜⬜⬜⬜⬜🟥🟩🟩🟩🟥⬜"
                             "⬜⬜⬜⬜⬜⬜⬜🟩⬛⬛⬛🟩⬜"
                             "⬜⬜🟪🟪🟪🟪🟪🟪🟪🟪⬛🟩⬜"
                             "⬜⬜🟪⬛⬛⬛⬛⬛⬛🟪⬛🟩⬜"
                             "⬜⬜🟪🟪🟪🟪🟪🟪🟪🟪⬛🟩⬜"
                             "⬜⬜⬜⬜⬜⬜⬜⬜⬜🟩⬛🟩⬜"
                             "⬜⬜⬜⬜⬜⬜⬜⬜⬜🟥🟩🟥⬜"
                             "⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜⬜"])
               (:grid result)))
        (is (= 24
               (:area result)))))))
