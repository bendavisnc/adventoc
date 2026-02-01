(ns adventoc.twentytwentyfive.eight.playground
  (:require [adventoc.helpers :refer [input]]
            [clojure.string :as string]
            [clojure.math :refer [pow]]))

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

(defn connection-group
  [connections]
  (loop [connectionz connections
         connects    [(first (first connections))]
         acc         #{}]
    (if (empty? connects)
      [connectionz acc]
      (recur (dissoc connectionz (first connects))
             (concat (rest connects) (connectionz (first connects)))
             (conj acc (first connects))))))

(defn connection-groups
  [connections]
  (loop [conns connections
         acc   []]
    (if (empty? conns)
      acc
      (let [[conns-next conn] (connection-group conns)]
        (recur conns-next (conj acc conn))))))

(defn closest-connections
  [coord-distances n]
  (loop [connectionz {}
         i 0]
    (if (> i n)
      connectionz
      ;;else
      (let [[[a b] _] (nth coord-distances i)]
        (recur (-> connectionz
                   (update a conj b)
                   (update b conj a))
               (inc i))))))

(defn coord-distances
  [coords coord-combos]
  (assert (vector? coords))
  (assert (vector? (first coords)))
  (->> coord-combos
       (mapv (fn [[a b]] [[a b] (distance (coords a) (coords b))]))
       (sort-by last)))

(defn connect
  [coords limit]
  (let [coord-combos      (all-coord-combos (count coords))
        coord-distancez   (coord-distances coords coord-combos)
        connectionz       (closest-connections coord-distancez
                                               (or (some-> limit
                                                           dec)
                                                   (count coords)))

        connection-groupz (connection-groups connectionz)]
    connection-groupz))

(defn connect-all
  [coords]
  (let [coord-count     (count coords)
        coord-combos    (all-coord-combos (count coords))
        coord-distancez (coord-distances coords coord-combos)]
    (loop [i 0
           connection-groupz nil]
      (if (and (= 1 (count connection-groupz))
               (= coord-count
                  (count (first connection-groupz))))
        (let [[[a b] _] (nth coord-distancez (dec i))]
          [[(nth coords a) (nth coords b)]
           (first connection-groupz)])
        (recur (inc i)
               (connection-groups (closest-connections coord-distancez i)))))))

(defn playground
  ([input {:keys [connection-count connect-all?]}]
   (let [coords (input->coords input)]
     (if connect-all?
       (let [[[[x _ _] [x2 _ _]] _] (connect-all coords)]
         (* x x2))
       ;; else
       (let [conns-groups (connect coords connection-count)
             top3         (take 3
                                (sort-by (comp #(* -1 %) count)
                                         conns-groups))]
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