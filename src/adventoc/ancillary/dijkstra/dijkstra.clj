(ns adventoc.ancillary.dijkstra.dijkstra
  (:require
   [clojure.string :as str]))

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

(defn print-distances [graph start]
  (let [distances (dijkstra graph start)]
    (doseq [[v d] distances]
      (println (format "Shortest distance from %s to %s: %s"
                       (str/upper-case (name start))
                       (str/upper-case (name v))
                       d)))))

(comment (let [m {:a 9 :b 2 :c 3}]
           (apply min-key m [:a :b :c])))
