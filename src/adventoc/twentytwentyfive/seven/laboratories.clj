(ns adventoc.twentytwentyfive.seven.laboratories
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(def s \S)
(def beam \|)
(def splinter \^)
(def vacancy \.)

(def index-of
  (fn [^String s c-or-s]
    (as-> (string/index-of s c-or-s) idx
      (when (not= idx -1) idx))))

(defn input->grid [input]
  (string/split-lines input))

(defn beam-at [row, idx]
  (apply str (map (fn [[i c]]
                    (if (= idx i)
                      beam
                      c))
               (map-indexed vector row))))

(defn beam-insert-step [grid, [x, y]]
  (let [top (fn [[x', y']]
              (and (= vacancy (get-in grid [y', x']))
                   (#{s, beam} (get-in grid [(dec y'), x']))))
        left (fn [[x', y']]
               (= splinter (get-in grid [y', (inc x')])))
        right (fn [[x', y']]
                (= splinter (get-in grid [y', (dec x')])))
        should-insert? ((some-fn top left right) [x, y])]
    (if should-insert?
      (update grid y #(beam-at % x))
      grid)))

(defn all-coords [[max-x, max-y]]
  (mapcat (fn [x]
            (map (fn [y]
                   [x, y])
                 (range max-y)))
          (range max-x)))

(defn insert-beams [grid]
  (let [max-x (count (first grid))
        max-y (count grid)]
    (reduce beam-insert-step grid (all-coords [max-x, max-y]))))

(defn beam-splinters-count [grid]
  (loop [how-many 0
         [headrow & restrows] grid
         acc nil]
    (if (nil? headrow)
      how-many
      (if-let [top-row (first acc)]
        (let [row-splinters (count (filter (fn [[i, c]]
                                             (= [beam splinter]
                                                [(nth top-row i) c]))
                                           (map-indexed vector headrow)))]
          (recur (+ how-many row-splinters)
                 restrows
                 (conj acc headrow)))
        (recur how-many
               restrows
               (conj acc headrow))))))

(defn laboratories [input]
  (let [grid (input->grid input)
        grid-with-beams (insert-beams grid)]
    (beam-splinters-count grid-with-beams)))

(defn -main [& args]
  (time (println
          (laboratories (input)))))
