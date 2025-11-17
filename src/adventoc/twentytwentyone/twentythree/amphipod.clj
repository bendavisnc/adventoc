(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]]
   [clojure.string :as string]
   [shams.priority-queue :as pq]))

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

(defn weight [journey]
  (* -1 (:accumulated-cost journey)))

(def journey-queue (pq/priority-queue weight))

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

(defn graph-rows-columns-switch [graph]
  (vec (for [row-i (range (count (first graph)))]
         (vec (for [column-i (range (count graph))]
                (get-in graph [column-i row-i]))))))

(defn str->burrow [s]
  (let [[amphipods-acc _]
        (reduce (fn [[acc, seen], c]
                  (let [amphipod-char (#{\A, \B, \C, \D} c)
                        amph (when amphipod-char
                               (some (fn [i]
                                       (let [a (keyword (str amphipod-char i))]
                                         (when (not (seen a))
                                           a)))
                                     (range ##Inf)))]
                    (if amph
                      [(conj acc amph) (conj seen amph)]
                      [acc, seen])))
                [[], #{}]
                s)
        amphipod-graph (->> amphipods-acc
                            (partition (count rooms))
                            (map vec)
                            vec
                            graph-rows-columns-switch)
        burrow* (-> burrow-empty
                    (assoc-in [:room :A] (vec (rseq (amphipod-graph 0))))
                    (assoc-in [:room :B] (vec (rseq (amphipod-graph 1)))))
        burrow (if (= 4 (count rooms))
                 (-> burrow*
                     (assoc-in [:room :C] (vec (rseq (amphipod-graph 2))))
                     (assoc-in [:room :D] (vec (rseq (amphipod-graph 3)))))
                 (-> burrow*
                     (update :room dissoc :C)
                     (update :room dissoc :D)))]
    burrow))

(defn journey-start [burrow]
  {:accumulated-cost 0
   :seen #{}
   :burrow burrow
   :moves (reduce (fn [acc, [room [occ-a, occ-b]]]
                    (-> acc
                        (conj [occ-a [:room room 0]])
                        (conj [occ-b [:room room 1]])))
                  []
                  (:room burrow))})

(defn energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn amphipod->room [amphipod]
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

(def positions-between
  (memoize
    (fn [from, to]
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
          (reverse (positions-between to from)))))))

(defn can-move? [journey, amphipod position]
  (let [burrow (:burrow journey)
        [_ current-position] (move-most-recent amphipod (:moves journey))
        [current-room, current-room-index] (when (= :room (first current-position))
                                             (rest current-position))
        [dest-room, _] (when (= :room (first position))
                         (rest position))
        other-index ({0 1 1 0} current-room-index)
        other-occupant (get-in burrow [:room current-room other-index])]
    (cond ;; Don't move into an occupied position.
          (not (nil? (get-in burrow position)))
          false
          ;; Don't leave home if you don't need to.
          (and (= 0 current-room-index)
               (= (amphipod->room amphipod) current-room))
          false
          ;; No need to get out of the way if other guy doesn't need to leave
          (and (= 1 current-room-index)
               (= (amphipod->room amphipod) (amphipod->room other-occupant) current-room))
          false
          ;; Can't move into home of wrong burrow
          (and (= :room (first position))
               (not= (amphipod->room amphipod) dest-room))
          false
          ;; Can't move into home of right burrow if occupied by wrong amphipod
          (and (= :room (first position))
               (not-every? (fn [occupant]
                             (or (nil? occupant)
                                 (= (amphipod->room amphipod)
                                    (amphipod->room occupant))))
                           [(get-in burrow [:room dest-room 0])
                            (get-in burrow [:room dest-room 1])]))
          false
          ;; Can't hover in front of own room.
          (and (= :hallway (first position))
               (= (second position)
                  (room-position (amphipod->room amphipod))))
          false
          ;; No one can be in the way.
          (not (= #{nil} (set (for [p (positions-between current-position
                                                         position)]
                                (get-in burrow p)))))
          false
          :else
          true)))

(defn goal [journey]
  (let [burrow (:burrow journey)
        hallway-empty? (= #{nil} (set (get burrow :hallway)))
        amphipods-all-home? (reduce
                              (fn [a, b] (and a b))
                              (map (fn [amphipod]
                                     (let [last-move (move-most-recent amphipod (:moves journey))
                                           move-position (second last-move)
                                           amphipod-room (when (= :room (first move-position))
                                                           (second move-position))
                                           home? (= (amphipod->room amphipod) amphipod-room)]
                                       home?))
                                   (set (map first (:moves journey)))))]
    (when (and hallway-empty? amphipods-all-home?)
      journey)))

(def distance (memoize (fn [from, to]
                         (assert (#{:room :hallway} (first from)) (str "Invalid `from` position: " from))
                         (assert (#{:room :hallway} (first to)) (str "Invalid `to` position: " to))
                         (case [(first from) (first to)]
                           [:hallway :hallway]
                           (let [hallway-from-index (second from)
                                 hallway-to-index (second to)]
                             (Math/abs (- hallway-from-index hallway-to-index)))
                           [:room :hallway]
                           (let [[_ room, room-index] from
                                 hallway-room-index (room room-position)
                                 back-room (if (zero? room-index) 1 0)]
                             (+ back-room 1 (distance [:hallway hallway-room-index] to)))
                           [:hallway :room]
                           (distance to from)
                           [:room :room]
                           (let [[_ room-from, room-from-index] from
                                 hallway-from-room-index (room-from room-position)
                                 back-from-room (if (zero? room-from-index) 1 0)
                                 [_ room-to, room-to-index] to
                                 hallway-to-room-index (room-to room-position)
                                 back-to-room (if (zero? room-to-index) 1 0)]
                             (if (= room-from room-to)
                               (Math/abs (- room-from-index room-to-index))
                               (+ back-from-room 1 back-to-room 1 (distance [:hallway hallway-from-room-index] [:hallway hallway-to-room-index]))))
                           (throw (ex-info (str "Unknown distance for from " from " to " to) {:from from :to to}))))))

(defn cost [amphipod from, to]
  (* (energy amphipod)
     (distance from to)))

(defn move-applied [journey move]
  (let [[amphipod move-position] move
        _ (assert (amphipods amphipod) (str "Unknown amphipod: " amphipod))
        move-position-start (second (move-most-recent amphipod (:moves journey)))
        _ (assert move-position-start (str "Amphipod " amphipod " has no starting position in journey moves."))
        move-cost (cost amphipod
                    move-position-start
                    move-position)]
    (-> journey
        (update :moves conj move)
        (update :cost conj move-cost)
        (update :accumulated-cost
                +
                move-cost))))

(defn assoc-burrow [journey]
  (assoc journey :burrow (journey->burrow journey)))

(defn journeys-afresh [journey]
  (for [amphipod amphipods
        move-placement move-placements
        :when (can-move? journey amphipod move-placement)
        :let [journey-next (-> journey
                               (move-applied [amphipod move-placement])
                               (assoc-burrow))]]
    journey-next))

(defn journey->breadcrumbs [journey]
  (assert (= (count (:cost journey))
             (count (:moves journey)))
          "Journey cost and moves length mismatch.")
  (string/join (flatten (for [i (range (count (:moves journey)))]
                          ["\n"
                           (str "cost: " (get-in journey [:cost i]))
                           "\n"
                           (burrow->str (journey->burrow (update journey
                                                                 :moves
                                                                 (fn [acc]
                                                                   (vec (take (inc i) acc))))))
                           "\n"]))))

(defn amphipod-solve [journey]
  (let [queue (conj journey-queue
                    (assoc-burrow journey))
        call-count (atom 0)]
    (loop [q queue
           seen #{}]
      (when (zero? (mod @call-count 1000))
        (println "Progress check:" @call-count
                 "queue size:" (count q)
                 "lowest cost:" (:accumulated-cost (peek q))))
      (swap! call-count inc)
      (let [journey-atm (peek q)
            burrow-atm (:burrow journey-atm)]
        (if (seen burrow-atm)
          (recur (pop q) seen)
          (let [journey-success (goal journey-atm)
                journeys-next (for [j (journeys-afresh journey-atm)
                                    :let [b (:burrow j)]
                                    :when (not (seen b))]
                                j)]
            (or journey-success
                (recur (reduce (fn [acc, s]
                                 (conj acc s))
                         (pop q)
                         journeys-next)
                  (conj seen burrow-atm)))))))))

(defn amphipod [puzzle]
  (-> puzzle str->burrow journey-start amphipod-solve :accumulated-cost))

(comment (rseq [1 2]))
