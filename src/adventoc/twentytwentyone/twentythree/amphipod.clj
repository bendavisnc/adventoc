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

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########

(defn burrow->str [burrow]
  (format "#############
#%s%s%s%s%s%s%s%s%s%s%s#
###%s#%s#%s#%s###
  #%s#%s#%s#%s#
  #########"
          (if-let [h0 (get-in burrow [:hallway 0])] (first (name h0)) ".")
          (if-let [h1 (get-in burrow [:hallway 1])] (first (name h1)) ".")
          (if-let [h2 (get-in burrow [:hallway 2])] (first (name h2)) ".")
          (if-let [h3 (get-in burrow [:hallway 3])] (first (name h3)) ".")
          (if-let [h4 (get-in burrow [:hallway 4])] (first (name h4)) ".")
          (if-let [h5 (get-in burrow [:hallway 5])] (first (name h5)) ".")
          (if-let [h6 (get-in burrow [:hallway 6])] (first (name h6)) ".")
          (if-let [h7 (get-in burrow [:hallway 7])] (first (name h7)) ".")
          (if-let [h8 (get-in burrow [:hallway 8])] (first (name h8)) ".")
          (if-let [h9 (get-in burrow [:hallway 9])] (first (name h9)) ".")
          (if-let [h10 (get-in burrow [:hallway 10])] (first (name h10)) ".")
          (if-let [a0 (get-in burrow [:room :A 0])] (first (name a0)) ".")
          (if-let [b0 (get-in burrow [:room :B 0])] (first (name b0)) ".")
          (if-let [c0 (get-in burrow [:room :C 0])] (first (name c0)) ".")
          (if-let [d0 (get-in burrow [:room :D 0])] (first (name d0)) ".")
          (if-let [a1 (get-in burrow [:room :A 1])] (first (name a1)) ".")
          (if-let [b1 (get-in burrow [:room :B 1])] (first (name b1)) ".")
          (if-let [c1 (get-in burrow [:room :C 1])] (first (name c1)) ".")
          (if-let [d1 (get-in burrow [:room :D 1])] (first (name d1)) ".")))

(defn energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn room [amphipod]
  (some-> amphipod name first str keyword))

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

(defn move-most-recent [amphipod moves]
  (some (fn [move]
          (when (= amphipod (first move))
            move))
        (rseq moves)))

(defn goal [journey]
  (println (format "Checking goal for journey with accumulated-cost %d and %d moves"
                   (:accumulated-cost journey)
                   (count (:moves journey))))
  (let [burrow (journey->burrow journey)
        _ (println (burrow->str burrow))
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
