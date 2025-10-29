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

(def move-placements (concat (map #(vector :hallway %) (range hallway-size))
                       (for [room rooms
                             depth [0 1]]
                         [:room room depth])))

(def room-position
  {:A 2
   :B 4
   :C 6
   :D 8})

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
                (a-or-period [:room :A 1])
                (a-or-period [:room :B 1])
                (a-or-period [:room :C 1])
                (a-or-period [:room :D 1])
                (a-or-period [:room :A 0])
                (a-or-period [:room :B 0])
                (a-or-period [:room :C 0])
                (a-or-period [:room :D 0])]))))

(defn energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn room [amphipod]
  (some-> amphipod name first str keyword))

(defn move-most-recent [amphipod moves]
  (or (some (fn [move]
              (when (= amphipod (first move))
                move))
            (rseq moves))
      (throw (ex-info (str "Amphipod `" amphipod "` has no moves.") {:amphipod amphipod
                                                                     :moves moves}))))

(defn journey->burrow [journey]
  (assert (some-> journey :moves seq) "Journey has no moves")
  (let [moves (-> journey :moves rseq)]
    (reduce (fn [acc, amphipod]
              (let [last-move (some (fn [move]
                                      (when (= amphipod (first move))
                                        move))
                                    moves)
                    move-position (second last-move)]
                (try
                  (assoc-in acc move-position amphipod)
                  (catch Exception e
                    (throw (ex-info (str "Error placing amphipod " amphipod " at position " move-position)
                                    {:amphipod amphipod
                                     :move-position move-position}
                                    e))))))
            burrow-empty
            amphipods)))

(defn positions-between [from, to]
  (let [hallway-from-index (or (when (= :hallway (first from))
                                 (second from))
                               (room-position (second from)))
        hallway-to-index (or (when (= :hallway (first to))
                               (second to))
                             (room-position (second to)))
        is-left-to-right? (<= hallway-from-index hallway-to-index)
        hallway-positions (when is-left-to-right?
                            (map (fn [i]
                                   [:hallway i])
                                 (range (if (= :hallway (first from))
                                          (inc hallway-from-index)
                                          hallway-from-index)
                                        (if (= :hallway (first to))
                                          hallway-to-index
                                          (inc hallway-to-index)))))
        from-room-position (when (= [:room 0] [(first from) (last from)])
                             [[:room (second from) 1]])
        to-room-position (when (= [:room 0] [(first to) (last to)])
                           [[:room (second to) 1]])]
    (if is-left-to-right?
      (concat from-room-position hallway-positions to-room-position)
      (reverse (positions-between to from)))))

(defn can-move? [journey, amphipod position]
  (let [burrow (journey->burrow journey)
        [_ current-position] (move-most-recent amphipod (:moves journey))
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
          ;; Can't hover in front of own room.
          (and (= :hallway (first position))
               (= (second position)
                  (room-position (room amphipod))))
          false
          ;; No one can be in the way.
          (not (= #{nil} (set (for [p (positions-between current-position
                                                         position)]
                                (get-in burrow p)))))
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
  (let [[amphipod move-position] move
        _ (assert (amphipods amphipod) (str "Unknown amphipod: " amphipod))
        move-position-start (second (move-most-recent amphipod (:moves journey)))
        _ (assert move-position-start (str "Amphipod " amphipod " has no starting position in journey moves."))]
    (-> journey
        (update :moves conj move)
        (update :accumulated-cost
                +
                (cost amphipod
                      move-position-start
                      move-position)))))

(defn journeys-afresh [journey, seen]
  (for [amphipod amphipods
        move-placement move-placements
        :when (can-move? journey amphipod move-placement)
        :let [journey-next (move-applied journey
                                         [amphipod move-placement])
              burrow-next (journey->burrow journey-next)]
        :when (not (seen burrow-next))]
    journey-next))

(defn amphipod-solve [journey]
  (let [queue (conj (pq/priority-queue weight)
                    journey)
        call-count (atom 0)
        ;; max-call-count ##Inf]
        max-call-count 1000]
    (loop [q queue
           seen #{}]
      (when (>= @call-count max-call-count)
        (dorun (map (fn [i]
                      (println (burrow->str (journey->burrow (update (peek q)
                                                                     :moves
                                                                     (fn [acc]
                                                                       (vec (take i acc))))))))
                    (range 1 (count (:moves (peek q))))))
        (throw (ex-info (str "Exceeded max call count of " max-call-count)
                        {:max-call-count max-call-count})))
      (swap! call-count inc)
      (println (count q) " journeys in queue, lowest cost so far: " (:accumulated-cost (peek q)))
      ;; (println (burrow->str (journey->burrow (peek q))))
      (if-let [journey-success (goal (peek q))]
        journey-success
        (recur (reduce (fn [acc, s]
                         (conj acc s))
                       (pop q)
                       (journeys-afresh (peek q) seen))
               (conj seen (journey->burrow (peek q))))))))

(defn minimain []
  (let [journey-start {:accumulated-cost 0
                       :moves [[:A0 [:room :B 0]]
                               [:A1 [:room :A 1]]
                               [:B0 [:room :A 0]]
                               [:B1 [:room :B 1]]]}]
    (binding [rooms (take 2 rooms)]
      (require 'adventoc.twentytwentyone.twentythree.amphipod :reload)
      (println (amphipod-solve journey-start)))))

(defn -main [& args]
  (if (= ["true"] args)
    (minimain)
    (let [journey-start {:accumulated-cost 0
                         :moves [[:A0 [:room :A 0]]
                                 [:B0 [:room :A 1]]
                                 [:D0 [:room :B 0]]
                                 [:C0 [:room :B 1]]
                                 [:C1 [:room :C 0]]
                                 [:B1 [:room :C 1]]
                                 [:A1 [:room :D 0]]
                                 [:D1 [:room :D 1]]]}]
      (println (amphipod-solve journey-start)))))

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########

(comment amphipods)

(comment energys)

(comment (conj #{1} 2))

(comment (for [x (range 10)
               :when (even? x)
               :when (> x 3)
               :let [y (* x 2)]]
           y))
