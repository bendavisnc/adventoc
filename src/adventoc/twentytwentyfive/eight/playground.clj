(ns adventoc.twentytwentyfive.eight.playground
  (:require
    [adventoc.helpers :refer [input]]
    [clojure.math :refer [pow]]
    [clojure.set :as set]
    [clojure.string :as string]))

(defn input->coords
  [input]
  (vec (for [line (string/split-lines input)]
         (vec (for [cell (string/split line #",")
                    :let [coord (parse-long cell)]]
                coord)))))

(defn all-coord-combos
  [n]
  (for [i (range n)
        j (range i)]
    [i j]))

(defn distance
  [[x1 y1 z1] [x2 y2 z2]]
  (pow (+ (pow (- x1 x2) 2)
          (pow (- y1 y2) 2)
          (pow (- z1 z2) 2))
       (/ 1 2)))

(defn coord-distances
  [coords coord-combos]
  (assert (vector? coords))
  (assert (vector? (first coords)))
  (->> coord-combos
       (mapv (fn [[a b]] [[a b] (distance (coords a) (coords b))]))
       (sort-by last)))

(defn connect
  [coords & [limit]]
  (let [coord-count     (count coords)
        coord-combos    (all-coord-combos (count coords))
        coord-distances (coord-distances coords coord-combos)]
    (loop [i 0
           connections {}
           connection-groups []
           connection-groups-count 0
           max-connection-group-size 0]
      (let [[[a b] _] (nth coord-distances i)
            a-group   (some-> a
                              connections
                              connection-groups)
            b-group   (some-> b
                              connections
                              connection-groups)]
        (cond
          (= i
             limit)
          {:connection-groups (filterv seq connection-groups)}
          (= coord-count
             max-connection-group-size)
          (let [[[a b] _] (nth coord-distances (dec i))]
            {:last-connection [[(nth coords a) (nth coords b)]]})
          ;; No existing groups to connect to
          (and (nil? a-group)
               (nil? b-group))
          (let [connection-group-index connection-groups-count]
            (recur (inc i)
                   (-> connections
                       (assoc a connection-group-index)
                       (assoc b connection-group-index))
                   (conj connection-groups #{a b})
                   (inc connection-groups-count)
                   max-connection-group-size))
          ;; connect a to b
          (nil? a-group)
          (let [connection-group-index    (connections b)
                connection-groups-updated (update-in connection-groups
                                                     [connection-group-index]
                                                     conj
                                                     a)]
            (recur
             (inc i)
             (assoc connections a connection-group-index)
             connection-groups-updated
             connection-groups-count
             (max max-connection-group-size
                  (count (connection-groups-updated connection-group-index)))))
          ;; connect b to a
          (nil? b-group)
          (let [connection-group-index    (connections a)
                connection-groups-updated (update-in connection-groups
                                                     [connection-group-index]
                                                     conj
                                                     b)]
            (recur
             (inc i)
             (assoc connections b connection-group-index)
             connection-groups-updated
             connection-groups-count
             (max max-connection-group-size
                  (count (connection-groups-updated connection-group-index)))))
          ;; merge a and b groups
          (not= a-group b-group)
          (let [connection-group-index (connections a)
                b-group (connection-groups (connections b))
                connection-groups-updated (-> connection-groups
                                              (update-in
                                               [connection-group-index]
                                               set/union
                                               b-group)
                                              (assoc (connections b) nil))]
            (recur (inc i)
                   (reduce (fn [acc c]
                             (assoc acc c connection-group-index))
                           connections
                           b-group)
                   connection-groups-updated
                   connection-groups-count
                   (max max-connection-group-size
                        (count (connection-groups-updated
                                connection-group-index)))))
          ;; just continue
          :else
          (recur (inc i)
                 connections
                 connection-groups
                 connection-groups-count
                 max-connection-group-size))))))

(defn playground
  ([input {:keys [connection-count connect-all?]}]
   (let [coords (input->coords input)]
     (if connect-all?
       (let [{:keys [last-connection]} (connect coords)
             [[[x _ _] [x2 _ _]] _]    last-connection]
         (* x x2))
       ;; else
       (let [{:keys [connection-groups]} (connect coords connection-count)
             top3 (take 3
                        (sort-by (comp #(* -1 %) count)
                                 connection-groups))]
         (apply *
                (map count
                     top3))))))
  ([input] (playground input {})))

(defn -main
  [& args]
  (time (println (let [connection-count (when-let
                                          [[_ n]
                                           (some->>
                                             (first args)
                                             (re-matches
                                              #".*--connection-count=(\d+).*"))]
                                          (parse-long n))
                       connect-all?     (some->> (first args)
                                                 (re-matches
                                                  #".*(--connect-all).*")
                                                 second
                                                 boolean)]
                   (playground (input)
                               {:connection-count connection-count
                                :connect-all?     connect-all?})))))


(comment
  (sort-by last [[:a :b] [:c :d]]))

(comment
  (let [groups      [#{1} #{2 3}]
        connections {1 0
                     3 1
                     2 1}
        a           1
        b           2
        a-group     0
        b-group     1]
    "merge a and b groups"
    "for every element in b group, update its connection to a group"))

(comment
  (set/union #{:a :b} #{:c :b}))

(comment
  (count #{0 7 1 4 15 13 6 17 3 12 2 19 11 9 5 14 16 10 18 8}))

