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

(defn all-coord-combos [n] (for [i (range n) j (range i)] [i j]))

(defn distance
  [[x1 y1 z1] [x2 y2 z2]]
  (pow (+ (pow (- x1 x2) 2) (pow (- y1 y2) 2) (pow (- z1 z2) 2)) (/ 1 2)))

(defn connection-group
  [connections]
  (loop [connectionz connections
         connects [(first (first connections))]
         acc #{}]
    (if (empty? connects)
      [connectionz acc]
      (recur (dissoc connectionz (first connects))
             (concat (rest connects) (connectionz (first connects)))
             (conj acc (first connects))))))

(defn connection-groups
  [connections]
  (loop [conns connections
         acc []]
    (if (empty? conns)
      acc
      (let [[conns-next conn] (connection-group conns)]
        (recur conns-next (conj acc conn))))))

(defn connections
  [coord-distances n]
  (loop [conns {}
         i 0]
    (if (>= i n)
      conns
      ;;else
      (let [[[a b] _] (nth coord-distances i)]
        (recur (-> conns
                   (update a conj b)
                   (update b conj a))
               (inc i))))))

(defn coord-distances
  [coords coord-combos]
  (sort-by last
           (for [[idx-a idx-b] coord-combos]
             [[idx-a idx-b] (distance (coords idx-a) (coords idx-b))])))

(defn playground
  ([input {:keys [connection-count]}]
   (let [coords (input->coords input)
         coord-combos (all-coord-combos (count coords))
         coord-distancez (coord-distances coords coord-combos)
         conns (connections coord-distancez
                            (or connection-count (count coords)))
         conns-groups (connection-groups conns)
         top3 (take 3 (sort-by (comp #(* -1 %) count) conns-groups))]
     (apply * (map count top3))))
  ([input] (playground input {})))

(defn -main
  [& args]
  (time (println (if-let [[_ n] (some->> (first args)
                                         (re-matches
                                          #"--connection-count=(\d+)"))]
                   (playground (input) {:connection-count (parse-long n)})
                   (playground (input))))))
