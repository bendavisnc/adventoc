(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]
   [shams.priority-queue :as pq]))

;; https://www.w3schools.com/dsa/trydsa.php?filename=demo_graphs_dijkstra
(defn dijkstra [graph start]
  (let [inf ##Inf
        vertices (keys graph)]
    (loop [dist (-> (into (sorted-map) (map vector vertices (repeat inf)))
                    (assoc start 0))
           visited #{}]
      (if (= visited (set vertices))
        dist
        (let [u (apply min-key dist (remove visited vertices))
              neighbors (graph u)
              dist' (reduce-kv (fn [d v w]
                                 (let [alt (+ (dist u) w)]
                                   (if (< alt (get d v inf))
                                     (assoc d v alt)
                                     d)))
                               dist
                               neighbors)]
          (recur dist' (conj visited u)))))))

;; https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Using_a_priority_queue
;;  1  function Dijkstra(Graph, source):
;;  2     
;;  3      for each vertex v in Graph.Vertices:
;;  4          dist[v] ← INFINITY
;;  5          prev[v] ← UNDEFINED
;;  6          add v to Q
;;  7      dist[source] ← 0
;;  8     
;;  9      while Q is not empty:
;; 10          u ← vertex in Q with minimum dist[u]
;; 11          Q.remove(u)
;; 12         
;; 13          for each arc (u, v) in Q:
;; 14              alt ← dist[u] + Graph.Edges(u, v)
;; 15              if alt < dist[v]:
;; 16                  dist[v] ← alt
;; 17                  prev[v] ← u
;; 18
;; 19      return dist[], prev[]
(defn dijkstra-q [graph start]
  (let [inf ##Inf
        vertices (keys graph)
        distances (-> (into (sorted-map) (map vector vertices (repeat inf)))
                      (assoc start 0))
        q (-> (pq/priority-queue (comp (fn [x] (some-> x (* -1)))
                                       second))
              (conj [start, 0]))]
    (loop [q q
           distances distances
           visited #{}]
      (if (empty? q)
        distances
        (let [[u, _] (peek q)
              q  (pop q)]
          (if (visited u)
            ;; Skip nodes we've already processed
            (recur q distances visited)
            (let [neighbors (graph u)
                  [distances' q'] (reduce-kv
                                    (fn [[d q] v w]
                                      (let [alt (+ (distances u) w)]
                                        (if (< alt (get d v inf))
                                          [(assoc d v alt) (conj q [v, alt])]
                                          [d q])))
                                    [distances q]
                                    neighbors)]
              (recur q' distances' (conj visited u)))))))))

(defn print-distances [graph start]
  (let [distances (dijkstra graph start)]
    (doseq [[v d] distances]
      (println (format "Shortest distance from %s to %s: %s"
                       (str/upper-case (name start))
                       (str/upper-case (name v))
                       d)))))
