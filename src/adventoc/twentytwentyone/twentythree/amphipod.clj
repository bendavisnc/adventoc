(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]]
   [shams.priority-queue :as pq])
  (:gen-class))

(def ^:dynamic rooms [:A :B :C :D])

(def energys (zipmap rooms (iterate #(* 10 %) 1)))

(def amphipods (apply sorted-set (for [room rooms
                                       index [0 1]]
                                   (keyword (str (name room) index)))))

(def hallway-size
  (case (count rooms)
    2 7
    4 11
    (throw (ex-info (str "Unknown hallway size for " (count rooms) " rooms") {:rooms rooms}))))

(def burrow-empty {:hallway (vec (repeat hallway-size nil))
                   :room {:A [nil, nil]
                          :B [nil, nil]
                          :C [nil, nil]
                          :D [nil, nil]}})

(def move-placements (concat (map #(vector :hallway %) (range 11))
                       (for [room rooms
                             depth [0 1]]
                         [:room room depth])))

(def room-position
  {:A 3
   :B 5
   :C 7
   :D 9})

(defn burrow->str [burrow]
  (let [a-or-period (fn [path]
                      (or (some-> (get-in burrow path)
                            name
                            first)
                          "."))]
    (apply format
      (flatten ["#############
#%s%s%s%s%s%s%s%s%s%s%s#
###%s#%s#%s#%s###
  #%s#%s#%s#%s#
  #########"
                (map (fn [i]
                       (a-or-period [:hallway i]))
                     (range 11))
                (a-or-period [:room :A 0])
                (a-or-period [:room :B 0])
                (a-or-period [:room :C 0])
                (a-or-period [:room :D 0])
                (a-or-period [:room :A 1])
                (a-or-period [:room :B 1])
                (a-or-period [:room :C 1])
                (a-or-period [:room :D 1])]))))

(defn energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn room [amphipod]
  (some-> amphipod name first str keyword))

(defn move-most-recent [amphipod moves]
  (some (fn [move]
          (when (= amphipod (first move))
            move))
        (rseq moves)))

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

(defn can-move? [journey, amphipod position]
  (let [burrow (journey->burrow journey)
        current-position (second (move-most-recent amphipod (:moves journey)))
        [current-room, current-room-index] (when (= :room (first current-position))
                                             (rest current-position))
        other-index ({0 1 1 0} current-room-index)
        other-occupant (get-in burrow [:room current-room other-index])]
    (cond ;; Don't move into an occupied position.
          (not (nil? (get-in burrow position)))
          false
          ;; Don't leave home if you don't need to.
          (and (= 0 current-room-index)
               (= (room amphipod) current-room))
          false
          ;; No need to get out of the way if other guy doesn't need to leave
          (and (= 1 current-room-index)
               (= (room amphipod) (room other-occupant) current-room))
          false
          :else
          true)))

(defn goal [journey]
  (println (format "Checking goal for journey with accumulated-cost %d and %d moves"
                   (:accumulated-cost journey)
                   (count (:moves journey))))
  (let [burrow (journey->burrow journey)
        ;; _ (println (burrow->str burrow))
        hallway-empty? (= #{nil} (set (get burrow :hallway)))
        amphipods-all-home? (reduce
                              (fn [a, b] (and a b))
                              (map (fn [amphipod]
                                     (let [last-move (move-most-recent amphipod (:moves journey))
                                           move-position (second last-move)
                                           amphipod-room (when (= :room (first move-position))
                                                           (second move-position))
                                           home? (= (room amphipod) amphipod-room)]
                                       home?))
                                   (set (map first (:moves journey)))))]
    (when (and hallway-empty? amphipods-all-home?)
      journey)))

(defn distance [from, to]
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
    [:hallway :hallway]
    (let [hallway-from-index (second from)
          hallway-to-index (second to)]
      (Math/abs (- hallway-from-index hallway-to-index)))
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
                  [amphipod move-placement])))

(defn amphipod-solve [journey]
  (let [queue (conj (pq/priority-queue weight)
                    journey)
        call-count (atom 0)
        max-call-count 100]
    (loop [q queue]
      (when (>= @call-count max-call-count)
        ;; (dorun (map (fn [j]
        ;;               (println (burrow->str (journey->burrow j))))
        ;;             (take 3 q)))
        (throw (ex-info (str "Exceeded max call count of " max-call-count)
                        {:max-call-count max-call-count})))
      (swap! call-count inc)
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
                               [:B1 [:room :B 0]]
                               [:A1 [:room :B 1]]]}]
    (binding [rooms (take 2 rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (println (amphipod-solve journey-start)))))

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########

(comment amphipods)

(comment energys)

(comment (= 1 1 1))
