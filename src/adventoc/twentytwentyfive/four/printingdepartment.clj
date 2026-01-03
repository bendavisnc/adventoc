(ns adventoc.twentytwentyfive.four.printingdepartment
  (:require
   [clojure.string :as string]))

(def paperroll \@)

(defn occupied? [graph, [x, y]]
  (boolean (#{paperroll} (get-in graph [y, x]))))

(defn adjacencies [[x y], [x-max, y-max]]
  (->> (for [dx [-1 0 1]
             dy [-1 0 1]
             :when (not (and (= dx 0) (= dy 0)))]
         [(+ x dx) (+ y dy)])
       (filter (fn [[x', y']]
                 (and (<= 0 x' (dec x-max))
                      (<= 0 y' (dec y-max)))))
       vec))

(defn increment-adjacents [counts, [x, y], [x-max, y-max]]
  (reduce (fn [acc, [x', y']]
            (update-in acc [x', y'] (fnil inc 0)))
          counts
          (adjacencies [x, y], [x-max, y-max])))

(defn adjacency-counts [graph]
  (let [row-size (count (first graph))
        column-size (count graph)]
    (reduce (fn [acc, [x, y]]
              (if (occupied? graph [x, y])
                (increment-adjacents acc [x, y] [row-size, column-size])
                acc))
            {}
            (for [row (range row-size)
                  column (range column-size)]
              [row, column]))))

(defn input->graph [input]
  (vec (for [row (vec (string/split-lines input))]
         (vec row))))

(defn counts-no-more-than [graph, counts, n]
  (let [row-size (count (first graph))
        column-size (count graph)]
    (reduce (fn [acc, [x, y]]
              (let [adjacency-count (or (get-in counts [x, y])
                                        0)]
                (if (< adjacency-count n)
                  (inc acc)
                  acc)))
            0
            (for [row (range row-size)
                  column (range column-size)
                  :when (occupied? graph [row, column])]
              [row, column]))))

(defn printingdepartment [input]
  (let [graph (input->graph input)
        counts (adjacency-counts graph)
        significant-adjacents-count (counts-no-more-than graph counts 4)]
    significant-adjacents-count))

(comment (vec (take 3 (repeat []))))

(comment (update [2, 2] 1 dec))

(comment (printingdepartment "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."))
