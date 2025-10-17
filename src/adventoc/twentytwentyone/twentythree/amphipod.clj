(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]]
   [shams.priority-queue :as pq])
  (:gen-class))

(def energys {:A 1
              :B 10
              :C 100
              :D 1000})

(def amphipods [:A0
                :A1
                :B0
                :B1
                :C0
                :C1
                :D0
                :D1])

(def burrow-empty {:hallway (vec (repeat 11 nil))
                   :room {:A [nil, nil]
                          :B [nil, nil]
                          :C [nil, nil]
                          :D [nil, nil]}})

(def positions (concat (map #(vector :hallway %) (range 11))
                       (for [room [:A :B :C :D]
                             depth [0 1]]
                         [:room room depth])))

(defn energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn can-move? [burrow amphipod position]
  true)

(defn moves [burrow]
  (for [amphipod amphipods
        position positions
        :when (can-move? burrow amphipod position)]
    [amphipod position]))

(defn goal? [step]
  (let [burrow (:burrow step)]
    (and (= #{:A0 :A1} (set (get-in burrow [:room :A])))
         (= #{:B0 :B1} (set (get-in burrow [:room :B])))
         (= #{:C0 :C1} (set (get-in burrow [:room :C])))
         (= #{:D0 :D1} (set (get-in burrow [:room :D])))
         (= #{nil} (set (get burrow :hallway))))))

(defn weight [steps]
  (reduce + (map :acc-energy steps)))

(defn amphipod-solve [burrow]
  ;; (let [step-zero [{:burrow burrow
  ;;                   :acc-energy 0}]
  ;;       queue (conj (pq/priority-queue weight)
  ;;                   step-zero)]
  ;;   (loop [q queue]
  ;;     (let [steps (steps-next (peek q))]
  ;;       (if-let [step (some goal? steps)]
  ;;         step
  ;;         (recur (reduce (fn [acc, s]
  ;;                          (conj acc s))
  ;;                        (pop q)
  ;;                        steps)))))))
  nil)

(defn journey->map [journey]
  (let [moves (-> journey :moves rseq)]
    (reduce (fn [acc, amphipod]
              (let [last-move (some (fn [move]
                                      (when (= amphipod (first move))
                                        move))
                                    moves)
                    move-position (rest last-move)]
                (assoc-in acc move-position amphipod)))
            burrow-empty
            amphipods)))

(defn distance [from, to]
  (case [(first from) (first to)]
    [:room :hallway]
    (let [hallway-index (second to)
          [_ room, room-index] from
          back-room (if (zero? room-index) 1 0)
          room-position (case room
                          :A 3
                          :B 5
                          :C 7
                          :D 9)]
      (+ (Math/abs (- hallway-index room-position))
         back-room))
    (throw (ex-info (str "Unknown distance for from " from " to " to) {:from from :to to}))))

(defn cost [amphipod from, to]
  (* (energy amphipod)
     (distance from to)))

(defn move-most-recent [amphipod moves]
  (some (fn [move]
          (when (= amphipod (first move))
            (rest move)))
        (rseq moves)))

(defn move-applied [step [amphipod move-to :as move]]
  (-> step
      (update :moves conj move)
      (update :accumulated-cost
              +
              (cost amphipod
                    (move-most-recent amphipod (:moves step))
                    move-to))))

(defn -main [& _]
  (let [burrow-journey-start {:accumulated-cost 0
                              :moves [[:A0 :room :A 0]
                                      [:B0 :room :A 1]
                                      [:D0 :room :B 0]
                                      [:C0 :room :B 1]
                                      [:C1 :room :C 0]
                                      [:B1 :room :C 1]
                                      [:A1 :room :D 0]
                                      [:D1 :room :D 1]]}]))

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########

(comment (-> (pq/priority-queue (fn [x] (* -1 x)))
             (conj 3)
             (conj 2)
             (conj 1)))
