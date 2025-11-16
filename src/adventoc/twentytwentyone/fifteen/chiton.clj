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

(defn graph-rows-columns-switch [graph]
  (vec (for [row-i (range (count (first graph)))]
         (vec (for [column-i (range (count graph))]
                (get-in graph [column-i row-i]))))))

(defn row-times [graph, times]
  (vec (for [row graph]
         (vec (flatten
                (for [times-i (range times)]
                  (for [v row
                        :let [v+ (+ v times-i)]]
                    (if (< 9 v+)
                      (+ (- v 10) times-i 1)
                      v+))))))))

(defn graph-times [graph, times]
  (-> graph
      (row-times times)
      (graph-rows-columns-switch)
      (row-times times)
      (graph-rows-columns-switch)))

(defn chiton [input & [times]]
  (let [graph (if times
                (-> input input->graph (graph-times times))
                (input->graph input))
        x-end (dec (count (first graph)))
        y-end (dec (count graph))
        {:keys [predecessors]} (dijkstra graph [0, 0])]
    (rest ;; don't count starting palce
      (solution-seq graph predecessors [x-end, y-end]))))

(comment (nextdoor [3, 3], [0, 1]))

(comment (< 0 1 2))

(comment (let [g [[0 1 2], [7 8 9]]]
           (get-in g [1 2])))


(comment (let [input (str/join "\n"
                               ["1163751742"
                                "1381373672"
                                "2136511328"
                                "3694931569"
                                "7463417111"
                                "1319128137"
                                "1359912421"
                                "3125421639"
                                "1293138521"
                                "2311944581"])
               graph (-> input input->graph (graph-times 5))
               s (str/join "\n"
                           (map #(str/join "" %)
                                graph))]
           (spit "wut.txt" s)))
