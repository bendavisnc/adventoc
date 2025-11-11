(ns adventoc.twentytwentyone.fifteen.chiton
  (:require
   [clojure.string :as str]
   [shams.priority-queue :as pq]))

(defn nextdoor [[x-max y-max], [x y]]
  (filter
    (fn [[nx ny]]
      (and (<= 0 nx x-max)
           (<= 0 ny y-max)))
    [[(dec x) y]
     [(inc x) y]
     [x (dec y)]
     [x (inc y)]]))

(defn dijkstra [graph start]
  (let [inf ##Inf
        x-max (dec (count (first graph)))
        y-max (dec (count graph))
        distances (assoc {} start 0)
        q (-> (pq/priority-queue (comp (fn [x] (some-> x (* -1)))
                                       second))
              (conj [start, 0]))]
    (loop [q q
           distances distances
           predecessors {}
           visited #{}]
      (if (empty? q)
        {:distances distances
         :predecessors predecessors}
        (let [[u, _] (peek q)
              q  (pop q)]
          (if (visited u)
            ;; Skip nodes we've already processed
            (recur q distances predecessors visited)
            (let [neighbors (nextdoor [x-max, y-max] u)
                  [distances' predecessors' q'] (reduce
                                                  (fn [[d p q] v]
                                                    (let [w (get-in graph (rseq v))
                                                            alt (+ (distances u) w)]
                                                      (if (< alt (get d v inf))
                                                        [(assoc d v alt) (assoc p v u) (conj q [v, alt])]
                                                        [d p q])))
                                                  [distances predecessors q]
                                                  neighbors)]
              (recur q' distances' predecessors' (conj visited u)))))))))

(defn input->graph [input]
  (vec (for [row (vec (str/split-lines input))]
         (mapv #(Integer/parseInt (str %)) row))))

(defn solution-seq [graph predecessors, xy-end]
  (loop [to xy-end
         acc []]
    (if-let [from (predecessors to)]
      (recur from (conj acc (get-in graph (rseq to))))
      (rseq (conj acc (get-in graph (rseq to)))))))

(defn chiton [input]
  (let [graph (input->graph input)
        x-end (dec (count (first graph)))
        y-end (dec (count graph))
        {:keys [predecessors]} (dijkstra graph [0, 0])]
    (solution-seq graph predecessors [x-end, y-end])))

(comment (nextdoor [3, 3], [0, 1]))

(comment (< 0 1 2))

(comment (let [g [[0 1 2], [7 8 9]]]
           (get-in g [1 2])))
