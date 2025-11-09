(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]))

;; https://www.w3schools.com/dsa/trydsa.php?filename=demo_graphs_dijkstra
(defn dijkstra [graph, start-vertex-data]
  (let [start-vertex (.indexOf (graph :vertex-data) start-vertex-data)
        distances (-> (vec (take (graph :size) (repeat ##Inf)))
                      (assoc start-vertex 0))
        visited (vec (take (graph :size) (repeat false)))]
    (loop [distances distances
           visited visited]
      (let [u (first (some (fn [a]
                             (when (not (last a))
                               a))
                           (sort-by second (map vector
                                                (range)
                                                distances
                                                visited))))]
        (if (not u)
          distances
          (recur (reduce (fn [ds v]
                           (if (and (not (zero? (get-in graph [:adj-matrix u, v])))
                                    (not (visited v)))
                             (let [alt (+ (distances u)
                                          (get-in graph [:adj-matrix u, v]))]
                               (if (< alt (distances v))
                                 (assoc ds v alt)
                                 ds))
                             ds))
                         distances
                         (range (graph :size)))
                 (assoc visited u true)))))))

(def size 7)
(def adj-matrix (-> (vec (repeat size (vec (repeat size 0))))
                    (assoc-in [3, 0] 4)
                    (assoc-in [3, 4] 2)
                    (assoc-in [0, 2] 3)
                    (assoc-in [0, 4] 4)
                    (assoc-in [4, 2] 4)
                    (assoc-in [4, 6] 5)
                    (assoc-in [2, 5] 5)
                    (assoc-in [2, 1] 2)
                    (assoc-in [1, 5] 2)
                    (assoc-in [6, 5] 5)))

(def vertex-data [:a, :b, :c, :d, :e, :f, :g])

(def graph {:size size
            :adj-matrix adj-matrix
            :vertex-data vertex-data})

(defn print-distances [graph, d]
  (let [distances (dijkstra graph d)]
    (dorun (for [[i, distance] (map-indexed vector distances)]
             (println (format "Shortest distance from %s to %s: %s"
                              (str/upper-case (name d))
                              (str/upper-case (name (get-in graph [:vertex-data i])))
                              distance))))))
(defn -main [& [d]]
  (print-distances graph (or (some-> d keyword)
                             :a)))

(comment (dijkstra graph :d))

(comment (str/upper-case (name :d)))

(comment (str adj-matrix))

(comment (second (vec (repeat size (vec (repeat size nil))))))

(comment (sort-by last (map vector (range) [:a, :b] [2, 1])))

(comment ((comp not last) [true false]))
