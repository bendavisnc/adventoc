(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]))

;; https://www.w3schools.com/dsa/trydsa.php?filename=demo_graphs_dijkstra
(defn dijkstra [graph start-vertex-data]
  (let [vertex-data (:vertex-data graph)
        adj-matrix  (:adj-matrix graph)
        size        (:size graph)
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

(def size 7)

(def adj-matrix
  (-> (vec (repeat size (vec (repeat size 0))))
      (assoc-in [3 0] 4)
      (assoc-in [3 4] 2)
      (assoc-in [0 2] 3)
      (assoc-in [0 4] 4)
      (assoc-in [4 2] 4)
      (assoc-in [4 6] 5)
      (assoc-in [2 5] 5)
      (assoc-in [2 1] 2)
      (assoc-in [1 5] 2)
      (assoc-in [6 5] 5)))

(def vertex-data [:a :b :c :d :e :f :g])

(def graph {:size size
            :adj-matrix adj-matrix
            :vertex-data vertex-data})

(defn print-distances [graph start]
  (let [distances (dijkstra graph start)]
    (doseq [[i d] (map-indexed vector distances)]
      (println (format "Shortest distance from %s to %s: %s"
                       (str/upper-case (name start))
                       (str/upper-case (name (get-in graph [:vertex-data i])))
                       d)))))

(defn -main [& [d]]
  (print-distances graph (or (some-> d keyword)
                             :a)))

(comment (dijkstra graph :d))

(comment (str/upper-case (name :d)))

(comment (str adj-matrix))

(comment (second (vec (repeat size (vec (repeat size nil))))))

(comment (sort-by last (map vector (range) [:a, :b] [2, 1])))

(comment ((comp not last) [true false]))

(comment (.equals ##Inf Double/POSITIVE_INFINITY))
