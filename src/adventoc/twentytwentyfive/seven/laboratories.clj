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

(defn insert-beams [grid]
  (loop [acc nil
         [row & more] grid]
    (if-not row
      (reverse acc)
      (let [start-idx        (index-of row s)
            left-idx         (index-of row (str vacancy splinter))
            right-idx        (index-of row (str splinter vacancy))
            top-row          (first acc)
            bottom-idx       (when top-row
                               (some (fn [[i c]]
                                       (when (= [beam vacancy] [(nth top-row i), c])
                                         i))
                                     (map-indexed vector row)))]
        (cond
          start-idx
          (let [next-row (first more)
                beam-row (beam-at next-row start-idx)]
            (recur
              (list beam-row row)
              (rest more)))

          left-idx
          (recur acc
                 (cons (beam-at row left-idx) more))
          right-idx
          (recur acc
                 (cons (beam-at row (inc right-idx)) more))

          bottom-idx
          (recur acc
                 (cons (beam-at row bottom-idx) more))
          :else
          (recur (cons row acc)
                 more))))))

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
