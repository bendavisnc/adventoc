(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]))

;; https://www.w3schools.com/dsa/trydsa.php?filename=demo_graphs_dijkstra
(defn dijkstra [graph start-vertex]
  (let [{:keys [vertex-data adj-matrix]} graph
        size       (count vertex-data)
        start-idx  (.indexOf vertex-data start-vertex)
        inf        Double/POSITIVE_INFINITY]

    (loop [distances (assoc (vec (repeat size inf)) start-idx 0)
           visited   (vec (repeat size false))]

      (let [unvisited (keep-indexed (fn [i d] (when-not (visited i) [i d])) distances)]
        (if (empty? unvisited)
          distances
          (let [[u-idx u-dist] (apply min-key second unvisited)
                updated-distances
                (reduce (fn [ds v]
                          (let [edge (get-in adj-matrix [u-idx v])]
                            (if (and edge (pos? edge))
                              (let [alt (+ u-dist edge)]
                                (if (< alt (ds v))
                                  (assoc ds v alt)
                                  ds))
                              ds)))
                        distances
                        (range size))]
            (recur updated-distances (assoc visited u-idx true))))))))

(defn print-distances [graph start]
  (let [distances (dijkstra graph start)]
    (doseq [[i d] (map-indexed vector distances)]
      (println (format "Shortest distance from %s to %s: %s"
                       (str/upper-case (name start))
                       (str/upper-case (name (get-in graph [:vertex-data i])))
                       d)))))
