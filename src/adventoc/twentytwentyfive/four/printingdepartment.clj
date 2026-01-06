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

(defn all-coords [[max-x, max-y]]
  (mapcat (fn [x]
            (map (fn [y]
                   [x, y])
                 (range max-y)))
          (range max-x)))

(defn size [grid]
  (if-let [row (first grid)]
    [(count row) (count grid)]
    (throw (ex-info "Empty grid" {:grid grid}))))

(defn adjacency-counts [grid]
  (let [bounds (size grid)]
    (reduce (fn [acc, [x, y]]
              (if (occupied? grid [x, y])
                (increment-adjacents acc [x, y] bounds)
                acc))
            {}
            (all-coords bounds))))

(defn input->grid [input]
  (vec (for [row (vec (string/split-lines input))]
         (vec row))))

(defn counts-no-more-than [grid, counts, n]
  (reduce (fn [acc, [x, y]]
            (let [adjacency-count (or (get-in counts [x, y])
                                      0)]
              (if (<= adjacency-count n)
                (inc acc)
                acc)))
          0
          (filter (partial occupied? grid)
                  (all-coords (size grid)))))

(defn grid-next [grid, counts, n]
  (vec (map-indexed (fn [y, row]
                      (vec (map-indexed (fn [x, cell]
                                          (cond (= paperroll-absent cell)
                                                paperroll-absent
                                                (<= (or (get-in counts [x, y])
                                                        0)
                                                    n)
                                                paperroll-absent
                                                :else
                                                paperroll))
                             row)))
         grid)))

(defn counts-no-more-than-continuous [grid, n]
  (loop [g grid
         acc 0]
    (let [counts (adjacency-counts g)
          removed (counts-no-more-than g counts n)]
      (if (zero? removed)
        acc
        (recur (grid-next g counts n)
               (+ acc removed))))))

(defn printingdepartment
  ([input, {:keys [continuous-removal?]}]
   (time
     (let [grid (input->grid input)]
       (if continuous-removal?
         (counts-no-more-than-continuous grid adjacency-limit)
         ;; else
         (let [counts (adjacency-counts grid)
               significant-adjacents-count (counts-no-more-than grid counts adjacency-limit)]
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
