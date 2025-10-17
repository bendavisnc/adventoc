(ns adventoc.twentytwentyone.twentythree.amphipod-test
  (:require
    [clojure.test :refer :all]
    [adventoc.twentytwentyone.twentythree.amphipod :as core]))


(def burrow-initial (-> core/burrow-empty
                        (assoc-in [:rooms :A] [:A0 :B0])
                        (assoc-in [:rooms :B] [:D0 :C0])
                        (assoc-in [:rooms :C] [:C1 :B1])
                        (assoc-in [:rooms :D] [:A1 :D1])))

(def burrow-journey-start {:accumulated-cost 0
                           :moves [[:A0 :room :A 0]
                                   [:B0 :room :A 1]
                                   [:D0 :room :B 0]
                                   [:C0 :room :B 1]
                                   [:C1 :room :C 0]
                                   [:B1 :room :C 1]
                                   [:A1 :room :D 0]
                                   [:D1 :room :D 1]]})

(deftest moves-test
  (testing "moves"
    (let [moves (core/moves burrow-initial)]
      (is (= [[:A0 [:hallway 0]] [:A0 [:hallway 1]] [:A0 [:hallway 2]]]
             (take 3 moves)))
      (is (= 152
             (count moves))))))

(deftest journey-to-map-test
  (testing "journey->map"
    (is (= {:hallway [nil nil nil nil nil nil nil nil nil nil nil], 
            :room {:A [:A0 :B0], 
                   :B [:D0 :C0], 
                   :C [:C1 :B1], 
                   :D [:A1 :D1]}}
           (core/journey->map burrow-journey-start)))))

(deftest distance-test
  (testing "distance"
    (let [distance (core/distance [:room :C 1] [:hallway 3])]
      (is (= 4 distance)))))

(deftest energy-test
  (testing "energy"
    (let [energy (core/energy :B1)]
      (is (= 10 energy)))))

(deftest cost-test
  (testing "cost"
    (let [cost (core/cost :B1 [:room :C 1] [:hallway 3])]
      (is (= 40 cost)))))

(deftest move-applied-test
  (testing "move-applied"
    (let [{:keys [moves accumulated-cost]} (core/move-applied burrow-journey-start [:B1 [:hallway 3]])]
      (is (= 40 accumulated-cost))
      (is (= [:B1 [:hallway 3]] (last moves))))))

