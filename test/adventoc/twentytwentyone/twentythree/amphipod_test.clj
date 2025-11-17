(ns adventoc.twentytwentyone.twentythree.amphipod-test
  (:require
   [adventoc.twentytwentyone.twentythree.amphipod :as core]
   [clojure.string :as string]
   [clojure.test :refer :all]))

(def puzzle (string/join "\n" ["#############"
                               "#...........#"
                               "###B#C#B#D###"
                               "  #A#D#C#A#"
                               "  #########"]))

(def puzzle-tiny (string/join "\n" ["#############"
                                    "#...........#"
                                    "###B#A#.#.###"
                                    "  #A#B#.#.#"
                                    "  #########"]))

(def journey-start (core/journey-start (core/str->burrow puzzle)))

(def journey-start-tiny (core/journey-start (core/str->burrow puzzle-tiny)))

(deftest journey-to-burrow-test
  (testing "journey->burrow"
    (is (= {:hallway [nil nil nil nil nil nil nil nil nil nil nil]
            :room {:A [:A0 :B0]
                   :B [:D1 :C0]
                   :C [:C1 :B1]
                   :D [:A1 :D0]}}
           (core/journey->burrow journey-start)))))

(deftest distance-test
  (testing "distance from hallway to hallway"
    (let [distance (core/distance [:hallway 0] [:hallway 3])]
      (is (= 3 distance))))
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
      (is (= 5 distance)))
    (let [distance (core/distance [:room :C 1] [:room :B 0])]
      (is (= 5 distance))))
  (testing "distance from room to same room"
    (let [distance (core/distance [:room :C 0] [:room :C 1])]
      (is (= 1 distance)))))

(deftest energy-test
  (testing "energy"
    (let [energies (map core/energy [:A0, :B0, :C0, :D0])]
      (is (= [1, 10, 100, 1000] energies)))))

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

(deftest positions-between-test
  (testing "positions-between, hallway to hallway"
    (let [positions (core/positions-between [:hallway 0] [:hallway 4])]
      (is (= [[:hallway 1] [:hallway 2] [:hallway 3]]
             positions))))
  (testing "positions-between, hallway to hallway, other direction"
    (let [positions (core/positions-between [:hallway 4] [:hallway 0])]
      (is (= (reverse (core/positions-between [:hallway 0] [:hallway 4]))
            positions))))
  (testing "positions-between, room to hallway"
    (let [positions (core/positions-between [:room :A 0] [:hallway 5])]
      (is (= [[:room :A 1] [:hallway 2] [:hallway 3] [:hallway 4]]
             positions))))
  (testing "positions-between, room to hallway, other direction"
    (let [positions (core/positions-between [:hallway 5] [:room :A 0])]
      (is (= (reverse (core/positions-between [:room :A 0] [:hallway 5]))
             positions))))
  (testing "positions-between, room to room"
    (let [positions (core/positions-between [:room :A 0] [:room :B 0])]
      (is (= [[:room :A 1] [:hallway 2] [:hallway 3] [:hallway 4] [:room :B 1]]
             positions))))
  (testing "positions-between, room to room, other direction"
    (let [positions (core/positions-between [:room :B 0] [:room :A 0])]
      (is (= (reverse (core/positions-between [:room :A 0] [:room :B 0]))
             positions)))))

(deftest can-move-test
  (testing "can-move? (to an empty space)"
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:B0 [:hallway 0]], [:A0 [:room :B 0]]]})
                        :A0
                        [:hallway 0])]
      (is (= false valid-move?)))
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:A0 [:room :B 0]]]})
                        :A0
                        [:hallway 0])]
      (is (= true valid-move?))))
  (testing "can-move? (not when already home)"
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:A0 [:room :A 0]]]})
                        :A0
                        [:hallway 0])]
      (is (= false valid-move?)))
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:A1 [:room :A 0]], [:A0 [:room :A 1]]]})
                        :A0
                        [:hallway 0])]
      (is (= false valid-move?)))
    (let [valid-move? (core/can-move?
                        {:accumulated-cost 0, :moves [[:A0 [:room :A 1]]]}
                        :A0
                        [:hallway 0])]
      (is (= true valid-move?))))
  (testing "can-move? (not in front of own base)"
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:B0 [:room :A 0]], [:A0 [:room :A 1]]]})
                        :A0
                        [:hallway 2])]
      (is (= false valid-move?))))
  (testing "can-move? (not when blocked)"
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:B0 [:hallway 6]], [:A0 [:room :B 0]]]})
                        :A0
                        [:room :D 0])]
      (is (= false valid-move?))))
  (testing "can-move? (not into non-home burrow)"
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:A0 [:hallway 0]]]})
                        :A0
                        [:room :D 0])]
      (is (= false valid-move?))))
  (testing "can-move? (not into home burrow, if currently occupied by different amphipod)"
    (let [valid-move? (core/can-move?
                        (core/assoc-burrow {:accumulated-cost 0, :moves [[:B0 [:room :A 0]] [:A0 [:hallway 0]]]})
                        :A0
                        [:room :A 1])]
      (is (= false valid-move?))))
  (testing "can-move? (not in front of own base, tiny)"
    (binding [core/rooms (take 2 core/rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (let [valid-move? (core/can-move?
                          (core/assoc-burrow {:accumulated-cost 0, :moves [[:B0 [:room :A 0]], [:A0 [:room :A 1]]]})
                          :A0
                          [:hallway 2])]
        (is (= false valid-move?))))
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)))

(deftest move-applied-test
  (testing "move-applied"
    (let [{:keys [moves accumulated-cost]} (core/move-applied journey-start
                                                              [:B1 [:hallway 3]])]
      (is (= 40 accumulated-cost))
      (is (= [:B1 [:hallway 3]] (last moves))))))

(deftest journey-afresh-test
  (testing "journeys-afresh"
    (let [journeys (core/journeys-afresh journey-start)]
      (is (= 37 (count journeys)))))
  (testing "journeys-afresh (tiny)"
    (binding [core/rooms (take 2 core/rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (let [journeys (core/journeys-afresh journey-start-tiny)]
        (is (= 24 (count journeys)))))
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)))

(deftest str->burrow-test
  (testing "str->burrow"
    (is (= {:hallway [nil nil nil nil nil nil nil nil nil nil nil]
            :room {:A [:A0 :B0], :B [:D1 :C0], :C [:C1 :B1], :D [:A1 :D0]}}
           (core/str->burrow puzzle))))
  (binding [core/rooms (take 2 core/rooms)]
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
    (testing "str->burrow, tiny"
      (is (= {:hallway [nil nil nil nil nil nil nil], :room {:A [:A1 :B0], :B [:B1 :A0]}}
             (core/str->burrow puzzle-tiny)))))
  (require 'adventoc.twentytwentyone.twentythree.amphipod :reload))

(deftest burrow->str-test
  (testing "burrow->str"
    (is (= puzzle
           (core/burrow->str (core/str->burrow puzzle)))))
  (binding [core/rooms (take 2 core/rooms)]
    (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
    (testing "burrow->str, tiny"
      (is (= puzzle-tiny
            (core/burrow->str (core/str->burrow puzzle-tiny))))))
  (require 'adventoc.twentytwentyone.twentythree.amphipod :reload))

(deftest journey-start-test
  (testing "journey-start"
    (is (= {:accumulated-cost 0, :seen #{}, :burrow {:hallway [nil nil nil nil nil nil nil nil nil nil nil], :room {:A [:A0 :B0], :B [:D1 :C0], :C [:C1 :B1], :D [:A1 :D0]}}, :moves [[:A0 [:room :A 0]] [:B0 [:room :A 1]] [:D1 [:room :B 0]] [:C0 [:room :B 1]] [:C1 [:room :C 0]] [:B1 [:room :C 1]] [:A1 [:room :D 0]] [:D0 [:room :D 1]]], :cost [0 0 0 0 0 0 0 0]}
           (core/journey-start (core/journey->burrow journey-start))))))

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
                              (update :moves conj [:D1 [:room :D 1]])
                              (core/assoc-burrow))
          goal (core/goal journey-success)]
      (is (= true (boolean goal)))))
  (testing "goal (smaller burrow)"
    (let [journey-success (core/assoc-burrow {:accumulated-cost 0
                                              :moves [[:A0 [:room :A 0]]
                                                      [:A1 [:room :A 1]]
                                                      [:B0 [:room :B 0]]
                                                      [:B1 [:room :B 1]]]})
          goal (core/goal journey-success)]
      (is (= true (boolean goal))))))

(deftest queue-test
  (testing "queue"
    (let [q (-> core/journey-queue
                (conj (update journey-start :accumulated-cost + 20))
                (conj (update journey-start :accumulated-cost + 10))
                (conj (update journey-start :accumulated-cost + 30)))]
      (is (= 3 (count q)))
      (is (= 10 (:accumulated-cost (peek q)))))))
