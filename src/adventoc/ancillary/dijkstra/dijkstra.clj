(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]))

;; https://www.w3schools.com/dsa/trydsa.php?filename=demo_graphs_dijkstra
(defn dijkstra [graph start-vertex-data]
  (let [vertex-data (:vertex-data graph)
        adj-matrix  (:adj-matrix graph)
        size        (count vertex-data)
        start-idx   (.indexOf vertex-data start-vertex-data)
        inf         Double/POSITIVE_INFINITY]

    (loop [distances (assoc (vec (repeat size inf)) start-idx 0)
           visited   (vec (repeat size false))]

      (let [unvisited (remove #(visited (first %))
                              (map-indexed vector distances))
            u         (when (seq unvisited)
                        (apply min-key second unvisited))]
        (if (nil? u)
          distances
          (let [[u-idx u-dist] u
                updated-distances
                (reduce (fn [ds v]
                          (let [edge (get-in adj-matrix [u-idx v])
                                alt  (if (= u-dist inf)
                                       inf
                                       (+ u-dist edge))]
                            (if (and (pos? edge)
                                  (not (visited v))
                                  (< alt (ds v)))
                              (assoc ds v alt)
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
