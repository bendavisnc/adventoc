(ns adventoc.twentytwentyfive.six.trashcompactor
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]
   [meander.epsilon :as m]))

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

(defn column->operand [c]
  (parse-long (string/trim (apply str c))))

(defn column->operator [c]
  (string->operator (str (first c))))

(defn process-step [acc, column]
  (m/match (merge acc
                  {:operator-next (column->operator column)
                   :operand-next (column->operand (reverse column))})
    ;; is there a new operator to apply?
    (m/and {:results ?results
            :operands ?operands
            :operator ?op}
           (m/guard (some? ?op)))
    {:operands nil
     :operator nil
     :results (conj ?results (apply ?op ?operands))}
    ;; is there a new operator to push (and maybe operand, same column)?
    {:operator-next ?op}
    (merge acc
           {:operator ?op
            :operands (if-let [operand (column->operand (reverse (rest column)))]
                        (conj (:operands acc) operand)
                        (:operands acc))})
    ;; is there a new operand to push
    {:operand-next ?operand}
    (update acc :operands conj ?operand)))

(defn process-math-ops-right->left [grid]
  (let [s {:operands nil
           :operator nil
           :results nil}
        columns (concat (reverse (map reverse grid)) ;; empty list to add one last step for performing the last operation.
                        (list []))]
    (:results (reduce process-step s columns))))

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
