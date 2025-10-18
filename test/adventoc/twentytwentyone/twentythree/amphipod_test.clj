(ns adventoc.twentytwentyone.twentythree.amphipod-test
  (:require
   [adventoc.twentytwentyone.twentythree.amphipod :as core]
   [clojure.test :refer :all]))

(def journey-start {:accumulated-cost 0
                    :moves [[:A0 [:room :A 0]]
                            [:B0 [:room :A 1]]
                            [:D0 [:room :B 0]]
                            [:C0 [:room :B 1]]
                            [:C1 [:room :C 0]]
                            [:B1 [:room :C 1]]
                            [:A1 [:room :D 0]]
                            [:D1 [:room :D 1]]]})

(deftest journey-to-burrow-test
  (testing "journey->burrow"
    (is (= {:hallway [nil nil nil nil nil nil nil nil nil nil nil]
            :room {:A [:A0 :B0]
                   :B [:D0 :C0]
                   :C [:C1 :B1]
                   :D [:A1 :D1]}}
           (core/journey->burrow journey-start)))))

(deftest distance-test
  (testing "distance from room to hallway"
    (let [distance (core/distance [:room :C 1] [:hallway 3])]
      (is (= 4 distance)))
    (let [distance (core/distance [:room :A 0] [:hallway 0])]
      (is (= 4 distance))))
  (testing "distance from hallway to room"
    (let [distance (core/distance [:hallway 3] [:room :C 1])]
      (is (= 4 distance))))
  (testing "distance from room to room"
    (let [distance (core/distance [:room :B 0] [:room :C 1])]
      (is (= 5 distance))))
  (testing "distance from room to same room"
    (let [distance (core/distance [:room :C 0] [:room :C 1])]
      (is (= 1 distance)))))

(deftest energy-test
  (testing "energy"
    (let [energy (core/energy :B1)]
      (is (= 10 energy)))))

(deftest cost-test
  (testing "cost"
    (let [cost (core/cost :B1 [:room :C 1] [:hallway 3])]
      (is (= 40 cost)))))

(deftest moves-most-recent-test
  (testing "move-most-recent"
    (let [journey (-> journey-start
                      (update :moves conj [:A0 [:hallway 0]]))
          move (core/move-most-recent :A0 (:moves journey))]
      (is (= [:A0 [:hallway 0]]
             move)))))

(deftest move-applied-test
  (testing "move-applied"
    (let [{:keys [moves accumulated-cost]} (core/move-applied journey-start
                                                              [:B1 [:hallway 3]])]
      (is (= 40 accumulated-cost))
      (is (= [:B1 [:hallway 3]] (last moves))))))

(deftest journey-afresh-test
  (testing "journeys-afresh"
    (let [journeys (core/journeys-afresh journey-start)]
      (is (= 152 (count journeys))))))

(deftest goal-test
  (testing "goal"
    (let [journey-success (-> journey-start
                              (update :moves conj [:A0 [:room :A 0]])
                              (update :moves conj [:A1 [:room :A 1]])
                              (update :moves conj [:B0 [:room :B 0]])
                              (update :moves conj [:B1 [:room :B 1]])
                              (update :moves conj [:C0 [:room :C 0]])
                              (update :moves conj [:C1 [:room :C 1]])
                              (update :moves conj [:D0 [:room :D 0]])
                              (update :moves conj [:D1 [:room :D 1]]))
          goal (core/goal journey-success)]
      (is (= true (boolean goal))))))
