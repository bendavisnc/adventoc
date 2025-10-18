(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]]
   [shams.priority-queue :as pq])
  (:gen-class))

(def energys {:A 1
              :B 10
              :C 100
              :D 1000})

(def amphipods (sorted-set :A0
                           :A1
                           :B0
                           :B1
                           :C0
                           :C1
                           :D0
                           :D1))

(def burrow-empty {:hallway (vec (repeat 11 nil))
                   :room {:A [nil, nil]
                          :B [nil, nil]
                          :C [nil, nil]
                          :D [nil, nil]}})

(def move-placements (concat (map #(vector :hallway %) (range 11))
                       (for [room [:A :B :C :D]
                             depth [0 1]]
                         [:room room depth])))

(def room-position
  {:A 3
   :B 5
   :C 7
   :D 9})

(defn energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn can-move? [burrow amphipod position]
  true)

(defn journey->burrow [journey]
  (assert (some-> journey :moves seq) "Journey has no moves")
  (let [moves (-> journey :moves rseq)]
    (reduce (fn [acc, amphipod]
              (let [last-move (some (fn [move]
                                      (when (= amphipod (first move))
                                        move))
                                    moves)
                    move-position (second last-move)]
                (assoc-in acc move-position amphipod)))
            burrow-empty
            amphipods)))

(defn goal [journey]
  (println (format "Checking goal for journey with accumulated-cost %d and %d moves"
                   (:accumulated-cost journey)
                   (count (:moves journey))))
  (let [burrow (journey->burrow journey)]
    (when (and (= #{:A0 :A1} (set (get-in burrow [:room :A])))
               (= #{:B0 :B1} (set (get-in burrow [:room :B])))
               (= #{:C0 :C1} (set (get-in burrow [:room :C])))
               (= #{:D0 :D1} (set (get-in burrow [:room :D])))
               (= #{nil} (set (get burrow :hallway))))
      journey)))

(defn distance [from, to]
  (when-not (#{:room :hallway} (first from))
    (assert false (str "Invalid (also) `from` position: " from)))
  (assert (#{:room :hallway} (first from)) (str "Invalid `from` position: " from))
  (assert (#{:room :hallway} (first to)) (str "Invalid `to` position: " to))
  (case [(first from) (first to)]
    [:room :hallway]
    (let [hallway-index (second to)
          [_ room, room-index] from
          back-room (if (zero? room-index) 1 0)]
      (+ (Math/abs (- hallway-index (room room-position)))
         back-room))
    [:hallway :room]
    (let [hallway-index (second from)
          [_ room, room-index] to
          back-room (if (zero? room-index) 1 0)]
      (+ (Math/abs (- hallway-index (room room-position)))
         back-room))
    [:room :room]
    (let [[_ room-from, room-from-index] from
          [_ room-to, room-to-index] to]
      (cond (= room-from room-to)
            (Math/abs (- room-from-index room-to-index))
            :else
            (+ (Math/abs (- (+ (room-from room-position))
                            (+ (room-to room-position))))
              room-from-index
              room-to-index
              2)))
    (throw (ex-info (str "Unknown distance for from " from " to " to) {:from from :to to}))))

(defn cost [amphipod from, to]
  (* (energy amphipod)
     (distance from to)))

(defn weight [journey]
  (* -1 (:accumulated-cost journey)))

(defn move-most-recent [amphipod moves]
  (some (fn [move]
          (when (= amphipod (first move))
            move))
        (rseq moves)))

(defn move-applied [journey move]
  (let [[amphipod move-position] move]
    (assert (amphipods amphipod) (str "Unknown amphipod: " amphipod))
    (-> journey
        (update :moves conj move)
        (update :accumulated-cost
                +
                (cost amphipod
                      (second (move-most-recent amphipod (:moves journey)))
                      move-position)))))

(defn journeys-afresh [journey]
  (for [amphipod amphipods
        move-placement move-placements
        :when (can-move? journey amphipod move-placement)]
    (move-applied journey
                  [amphipod
                   move-placement])))

(defn amphipod-solve [journey]
  (let [queue (conj (pq/priority-queue weight)
                    journey)]
    (loop [q queue]
      (println (count q) " journeys in queue, lowest cost so far: " (:accumulated-cost (peek q)))
      (if-let [journey-success (goal (peek q))]
        journey-success
        (recur (reduce (fn [acc, s]
                         (conj acc s))
                       (pop q)
                       (journeys-afresh (peek q))))))))

(defn -main [& _]
  (let [journey-start {:accumulated-cost 0
                       :moves [[:A0 [:room :A 0]]
                               [:B0 [:room :A 1]]
                               [:D0 [:room :B 0]]
                               [:C0 [:room :B 1]]
                               [:C1 [:room :C 0]]
                               [:B1 [:room :C 1]]
                               [:A1 [:room :D 0]]
                               [:D1 [:room :D 1]]]}]
    (println (amphipod-solve journey-start))))

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########

;; (comment (let [j {:accumulated-cost 0
;;                   :moves [[:A0 :room :A 0]
;;                           [:B0 :room :A 1]
;;                           [:D0 :room :B 0]
;;                           [:C0 :room :B 1]
;;                           [:C1 :room :C 0]
;;                           [:B1 :room :C 1]
;;                           [:A1 :room :D 0]
;;                           [:D1 :room :D 1]]}
