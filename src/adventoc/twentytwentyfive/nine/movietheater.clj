(ns adventoc.twentytwentyfive.nine.movietheater
  (:require
    [adventoc.helpers :refer [input]]
    [clojure.string :as string]))

(defn input->coords
  [input]
  (vec (for [line (string/split-lines input)]
         (vec (for [cell (string/split line #",")
                    :let [coord (parse-long cell)]]
                coord)))))

(defn all-coord-combos
  [n]
  (for [i (range n)
        j (range i)]
    [i j]))

(defn area
  [[x1 y1] [x2 y2]]
  (* (inc (abs (- x2 x1)))
     (inc (abs (- y2 y1)))))


(defn coord-distances
  [coords coord-combos]
  (assert (vector? coords))
  (assert (vector? (first coords)))
  (->> coord-combos
       (mapv (fn [[a b]] [[a b] (area (coords a) (coords b))]))
       (sort-by (comp #(* -1 %)
                      last))))

(defn movietheater
  [input]
  (let [coords           (input->coords input)
        coord-combs      (all-coord-combos (count coords))
        distances        (coord-distances coords coord-combs)
        [_ largest-area] (first distances)]
    largest-area))

(defn -main
  [& args]
  (time (println (movietheater (input)))))
