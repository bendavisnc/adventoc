(ns adventoc.twentytwentyfive.six.trashcompactor
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(defn input->grid [input]
  (vec (for [row (vec (string/split-lines input))]
         (vec (for [cell (string/split row #"\s+")
                    :when (seq cell)]
                cell)))))

(defn input->grid-keep-whitespace [input]
  (vec (for [row (vec (string/split-lines input))]
         (vec row))))

(defn grid-rows-columns-switch [grid]
  (vec (for [row-i (range (count (first grid)))]
         (vec (for [column-i (range (count grid))]
                (get-in grid [column-i row-i]))))))

(defn string->operator [s]
  (case s
    "+" +
    "*" *
    nil))

(defn process-math-ops [grid]
  (map (fn [operands-and-operator]
         (let [[op & operands] (rseq operands-and-operator)]
           (apply (string->operator op)
                  (map parse-long operands))))
       grid))

(defn column->operand [acc]
  (parse-long (string/trim (apply str acc))))

(defn column->operator [acc]
  (string->operator (str (first acc))))

(defn process-math-ops-right->left [grid]
  (loop [operands nil
         operator nil
         operation-results nil
         raw-columns (reverse (map reverse grid))]
    (cond operator
          (recur nil
                 nil
                 (conj operation-results (apply operator operands))
                 raw-columns)
          (empty? raw-columns)
          operation-results
          (column->operator (first raw-columns))
          (let [column (first raw-columns)]
            (recur (if-let [oper (column->operand (reverse (rest column)))]
                     (conj operands oper)
                     operands)
                   (column->operator column)
                   operation-results
                   (rest raw-columns)))
          :else
          (let [column (first raw-columns)]
            (recur (if-let [oper (column->operand (reverse column))]
                     (conj operands oper)
                     operands)
                   nil
                   operation-results
                   (rest raw-columns))))))

(defn trashcompactor
  ([input {:keys [right-to-left]}]
   (let [sums
         (if right-to-left
           (process-math-ops-right->left (grid-rows-columns-switch (input->grid-keep-whitespace input)))
           (process-math-ops (grid-rows-columns-switch (input->grid input))))]
     (apply + sums)))
  ([input]
   (trashcompactor input {})))

(defn -main [& args]
  (time (println
          (if (= ["--right-to-left"] args)
            (trashcompactor (input) {:right-to-left true})
            (trashcompactor (input))))))
