(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]))

;; https://www.w3schools.com/dsa/trydsa.php?filename=demo_graphs_dijkstra
(defn dijkstra [graph start-vertex]
  (let [{:keys [vertex-data adj-matrix]} graph
        inf        Double/POSITIVE_INFINITY]
    (loop [distances (-> {}
                         (into (map vector vertex-data (repeat inf)))
                         (assoc start-vertex 0))
           visited   #{}]
      (let [unvisited (filter (fn [[v _]] (not (visited v))) distances)]
        (if (empty? unvisited)
          distances
          (let [[u-vert u-dist] (apply min-key second unvisited)
                updated-distances
                (reduce (fn [ds v]
                          (let [edge (get-in adj-matrix [u-vert v])]
                            (if (and edge (pos? edge))
                              (let [alt (+ u-dist edge)]
                                (if (< alt (ds v))
                                  (assoc ds v alt)
                                  ds))
                              ds)))
                        distances
                        vertex-data)]
            (recur updated-distances (conj visited u-vert))))))))

(defn print-distances [graph start]
  (let [distances (dijkstra graph start)]
    (doseq [[v d] distances]
      (println (format "Shortest distance from %s to %s: %s"
                       (str/upper-case (name start))
                       (str/upper-case (name v))
                       d)))))

(comment (let [m {:a 9 :b 2 :c 3}]
           (apply min-key second m)))
