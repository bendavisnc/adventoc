(ns adventoc.twentytwentyfive.six.trashcompactor
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(defn input->grid [input]
  (vec (for [row (vec (string/split-lines input))]
         (vec (for [cell (string/split row #"\s+")
                    :when (seq cell)]
                cell)))))

(defn grid-rows-columns-switch [grid]
  (vec (for [row-i (range (count (first grid)))]
         (vec (for [column-i (range (count grid))]
                (get-in grid [column-i row-i]))))))

(defn string->operator [s]
  (case s
    "+" +
    "*" *))

(defn process-math-ops [grid]
  (map (fn [operands-and-operator]
         (let [[op & operands] (rseq operands-and-operator)]
           (apply (string->operator op)
                  (map parse-long operands))))
       grid))

(defn trashcompactor [input]
  (time (println (apply + (process-math-ops (grid-rows-columns-switch (input->grid input)))))))

(defn -main [& args]
  (trashcompactor (input)))
