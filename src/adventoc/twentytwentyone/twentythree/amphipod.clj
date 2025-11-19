(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]]
   [clojure.string :as string]
   [shams.priority-queue :as pq]))

(def ^:dynamic rooms [:A :B :C :D])

(def ^:dynamic room-size 2)

(def energys (zipmap rooms (iterate #(* 10 %) 1)))

(def amphipods (apply sorted-set (for [room rooms
                                       index (range room-size)]
                                   (keyword (str (name room) index)))))

(def hallway-size
  (case (count rooms)
    2 7
    4 11
    (throw (ex-info (str "Unknown hallway size for " (count rooms) " rooms") {:rooms rooms}))))

(def burrow-empty (case (count rooms)
                    2
                    {:hallway (vec (repeat hallway-size nil))
                     :room {:A (vec (repeat room-size nil))
                            :B (vec (repeat room-size nil))}}
                    4
                    {:hallway (vec (repeat hallway-size nil))
                     :room {:A (vec (repeat room-size nil))
                            :B (vec (repeat room-size nil))
                            :C (vec (repeat room-size nil))
                            :D (vec (repeat room-size nil))}}))

(def move-placements (concat
                       (map #(vector :hallway %) (range hallway-size))
                       (for [room rooms
                             depth (range room-size)]
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
    (case room-size
      2
      (apply format
        (flatten [(string/join "\n" ["#############"
                                     "#%s%s%s%s%s%s%s%s%s%s%s#"
                                     "###%s#%s#%s#%s###"
                                     "  #%s#%s#%s#%s#"
                                     "  #########"])
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
                  (a-or-period [:room :D 0])]))
      4
      (apply format
        (flatten [(string/join "\n" ["#############"
                                     "#%s%s%s%s%s%s%s%s%s%s%s#"
                                     "###%s#%s#%s#%s###"
                                     "  #%s#%s#%s#%s#"
                                     "  #%s#%s#%s#%s#"
                                     "  #%s#%s#%s#%s#"
                                     "  #########"])
                  (map (fn [i]
                         (a-or-period [:hallway i]))
                    (range 11))
                  (a-or-period [:room :A 3])
                  (a-or-period [:room :B 3])
                  (a-or-period [:room :C 3])
                  (a-or-period [:room :D 3])
                  (a-or-period [:room :A 2])
                  (a-or-period [:room :B 2])
                  (a-or-period [:room :C 2])
                  (a-or-period [:room :D 2])
                  (a-or-period [:room :A 1])
                  (a-or-period [:room :B 1])
                  (a-or-period [:room :C 1])
                  (a-or-period [:room :D 1])
                  (a-or-period [:room :A 0])
                  (a-or-period [:room :B 0])
                  (a-or-period [:room :C 0])
                  (a-or-period [:room :D 0])])))))

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
        burrow (case (count rooms)
                 2
                 (-> burrow-empty
                     (assoc-in [:room :A] (vec (rseq (amphipod-graph 0))))
                     (assoc-in [:room :B] (vec (rseq (amphipod-graph 1)))))
                 4
                 (-> burrow-empty
                     (assoc-in [:room :A] (vec (rseq (amphipod-graph 0))))
                     (assoc-in [:room :B] (vec (rseq (amphipod-graph 1))))
                     (assoc-in [:room :C] (vec (rseq (amphipod-graph 2))))
                     (assoc-in [:room :D] (vec (rseq (amphipod-graph 3))))))]
    burrow))

(defn journey-start [burrow]
  (let [moves (case room-size
                2
                (reduce (fn [acc, [room [occ-a, occ-b]]]
                          (-> acc
                              (conj [occ-a [:room room 0]])
                              (conj [occ-b [:room room 1]])))
                        []
                        (:room burrow))
                4
                (reduce (fn [acc, [room [occ-a, occ-b, occ-c, occ-d]]]
                          (-> acc
                              (conj [occ-a [:room room 0]])
                              (conj [occ-b [:room room 1]])
                              (conj [occ-c [:room room 2]])
                              (conj [occ-d [:room room 3]])))
                        []
                        (:room burrow)))]
    {:accumulated-cost 0
     :seen #{}
     :burrow burrow
     :moves moves
     :cost (vec (repeat (count moves) 0))}))

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
            from-room-positions (when (= :room (first from))
                                  (vec (for [i (range (inc (last from)) room-size)]
                                         [:room (second from) i])))
            to-room-positions (when (= :room (first to))
                                (vec (for [i (range (inc (last to)) room-size)]
                                       [:room (second to) i])))]
        (if is-left-to-right?
          (concat from-room-positions hallway-positions to-room-positions)
          (reverse (positions-between to from)))))))

(defn can-move? [journey, amphipod position]
  (let [burrow (:burrow journey)
        [_ current-position] (move-most-recent amphipod (:moves journey))
        [current-room, current-room-index] (when (= :room (first current-position))
                                             (rest current-position))
        [dest-room, dest-room-index] (when (= :room (first position))
                                       (rest position))]
    (cond
      ;; Don't move into an occupied position.
      (not (nil? (get-in burrow position)))
      false
      ;; Don't move from hallway to hallway
      (not (or current-room dest-room))
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
                       (for [index (range room-size)]
                         (get-in burrow [:room dest-room index]))))
      false
      ;; Don't settle down in front of empty spots
      (and (= :room (first position))
           (some nil?
             (for [index (range dest-room-index)]
               (get-in burrow [:room dest-room index]))))
      false
      ;; No need to get out of the way if other guy doesn't need to leave
      (and (= current-room (amphipod->room amphipod))
           (every? (fn [occupant]
                     (or (nil? occupant)
                         (= (amphipod->room amphipod)
                            (amphipod->room occupant))))
             (for [index (range room-size)]
               (get-in burrow [:room current-room index]))))
      false
      ;; Can't hover in front of any room.
      (and (= :hallway (first position))
           (some (fn [hallway-entry-index]
                   (= (second position) hallway-entry-index))
                 (vals room-position)))
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
        amphipods-all-home? (= (case (count rooms)
                                 2
                                 [#{:A} #{:B}]
                                 4
                                 [#{:A} #{:B} #{:C} #{:D}])
                               (for [room rooms]
                                 (set (map amphipod->room (get-in burrow [:room room])))))]
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
                                 room-distance (- room-size room-index 1)]
                             (+ room-distance 1 (distance [:hallway hallway-room-index] to)))
                           [:hallway :room]
                           (distance to from)
                           [:room :room]
                           (let [[_ room-from, room-from-index] from
                                 hallway-from-room-index (room-from room-position)
                                 from-room-distance (- room-size room-from-index 1)
                                 [_ room-to, room-to-index] to
                                 hallway-to-room-index (room-to room-position)
                                 to-room-distance (- room-size room-to-index 1)]
                             (if (= room-from room-to)
                               (Math/abs (- room-from-index room-to-index))
                               (+ from-room-distance 1 to-room-distance 1 (distance [:hallway hallway-from-room-index] [:hallway hallway-to-room-index]))))
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
        (update-in [:move-counts amphipod] (fnil inc 0))
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
  (when-not (= (count (:cost journey))
               (count (:moves journey)))
            (throw (ex-info "Journey cost and moves length mismatch"
                            {:cost-length (count (:cost journey))
                             :moves-length (count (:moves journey))})))
  (string/join (flatten (for [i (range (count (:moves journey)))
                              :when (pos-int? (get-in journey [:cost i]))]
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
      (when (zero? (mod @call-count 100000))
        (println "Progress check:" @call-count
                 "queue size:" (count q)
                 "lowest cost:" (:accumulated-cost (peek q))
                 "step size:" (count (:moves (peek q)))
                 (journey->breadcrumbs (peek q))))
      (swap! call-count inc)
      (let [journey-atm (peek q)
            burrow-atm (:burrow journey-atm)]
        (if (or (seen burrow-atm))
                ;; (some (fn [[_, move-count]]
                ;;         (< 2 move-count))
                ;;       (some-> journey-atm :move-counts)))
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
  (let [solved (-> puzzle str->burrow journey-start amphipod-solve)]
    (println (journey->breadcrumbs solved))
    (:accumulated-cost solved)))

(comment (rseq [1 2]))
