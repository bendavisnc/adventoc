(ns adventoc.twentytwentyfive.four.printingdepartment
  (:require
   [clojure.string :as string]))

(def paperroll \@)

(def paperroll-absent \.)

(def adjacency-limit 3)

(defn occupied? [grid, [x, y]]
  (boolean (#{paperroll} (get-in grid [y, x]))))

(defn adjacencies [[x y], [max-x, max-y]]
  (let [deltas [-1 0 1]]
    (mapcat
      (fn [dx]
        (mapcat
          (fn [dy]
            (let [cx (+ x dx)
                  cy (+ y dy)]
              (if (and (not (and (= dx 0) (= dy 0)))
                       (>= cx 0) (< cx max-x)
                       (>= cy 0) (< cy max-y))
                [[cx cy]]
                [])))
          deltas))
      deltas)))

(defn increment-adjacents [counts, [x, y], [x-max, y-max]]
  (reduce (fn [acc, [x', y']]
            (update-in acc [x', y'] (fnil inc 0)))
          counts
          (adjacencies [x, y], [x-max, y-max])))

(defn adjacency-counts [grid]
  (let [row-size (count (first grid))
        column-size (count grid)]
    (reduce (fn [acc, [x, y]]
              (if (occupied? grid [x, y])
                (increment-adjacents acc [x, y] [row-size, column-size])
                acc))
            {}
            (for [row (range row-size)
                  column (range column-size)]
              [row, column]))))

(defn input->grid [input]
  (vec (for [row (vec (string/split-lines input))]
         (vec row))))

(defn counts-less-than [grid, counts, n]
  (let [row-size (count (first grid))
        column-size (count grid)]
    (reduce (fn [acc, [x, y]]
              (let [adjacency-count (or (get-in counts [x, y])
                                        0)]
                (if (<= adjacency-count n)
                  (inc acc)
                  acc)))
            0
            (for [row (range row-size)
                  column (range column-size)
                  :when (occupied? grid [row, column])]
              [row, column]))))

(defn grid-next [grid, counts, n]
  (let [row-size (count (first grid))
        column-size (count grid)]
    (reduce (fn [acc, [x, y]]
              (let [adjacency-count (or (get-in counts [x, y])
                                        0)
                    marker (cond (not (occupied? grid [x, y]))
                                 paperroll-absent
                                 (<= adjacency-count n)
                                 paperroll-absent
                                 :else
                                 (get-in grid [y, x]))]
                (assoc-in acc [y, x] marker)))
            (vec (take row-size (repeat [])))
            (for [row (range row-size)
                  column (range column-size)]
              [row, column]))))

(defn counts-no-more-than-continuous [grid, n]
  (loop [g grid
         acc 0
         last-count nil]
    (if (> 1 (or last-count ##Inf))
      (+ acc last-count)
      (let [acs (adjacency-counts g)
            c (counts-less-than g acs n)
            acc-next (+ acc (or last-count 0))
            g-next (grid-next g acs n)]
        (recur g-next acc-next c)))))

(defn printingdepartment
  ([input, {:keys [continuous-removal?]}]
   (time
     (let [grid (input->grid input)]
       (if continuous-removal?
         (counts-no-more-than-continuous grid adjacency-limit)
         ;; else
         (let [counts (adjacency-counts grid)
               significant-adjacents-count (counts-less-than grid counts adjacency-limit)]
           significant-adjacents-count)))))
  ([input]
   (printingdepartment input {})))

(comment (printingdepartment "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@." {:continuous-removal? true}))
