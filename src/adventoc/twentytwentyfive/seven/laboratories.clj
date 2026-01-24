(ns adventoc.twentytwentyfive.seven.laboratories
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string :refer [index-of]]))

(def s \S)
(def beam \|)
(def splinter \^)
(def vacancy \.)

(defn input->grid [input]
  (string/split-lines input))

(defn beam-at [row, idx]
  (apply str (map (fn [[i c]]
                    (if (= idx i)
                      beam
                      c))
               (map-indexed vector row))))

(defn all-coords [[max-x, max-y]]
  (mapcat (fn [y]
            (map (fn [x]
                   [x, y])
                 (range max-x)))
          (range max-y)))

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

(defn insert-beams [grid]
  (let [max-x (count (first grid))
        max-y (count grid)]
    (reduce beam-insert-step grid (all-coords [max-x, max-y]))))

(defn beam-splinters-count [grid]
  (reduce (fn [acc row-idx]
            (+ acc
               (count (filter (fn [ab]
                                (= [beam, splinter] ab))
                              (map vector
                                   (nth grid (dec row-idx))
                                   (nth grid row-idx))))))
          0
          (range 1 (count grid))))

(defn grid->branching-per-column-step [acc, row]
  (if (empty? acc)
    {(index-of row s) 1}
    (reduce (fn [s, i]
              (if (= splinter (nth row i))
                (-> s
                    (update (dec i) (fnil + 0) (acc i))
                    (update (inc i) (fnil + 0) (acc i)))
                (update s i (fnil + 0) (acc i))))
            {}
            (keys acc))))

(defn grid->branching-per-column [grid]
  (reduce grid->branching-per-column-step {} grid))

(defn laboratories
  ([input {:keys [quantum]}]
   (let [grid (input->grid input)]
     (if quantum
       (apply + (vals (grid->branching-per-column grid)))
       (beam-splinters-count (insert-beams grid)))))
  ([input]
   (laboratories input {})))

(defn -main [& args]
  (time (println
          (if (= ["--quantum"] args)
            (laboratories (input) {:quantum true})
            (laboratories (input))))))

(comment (map vector (list 1 2) (list 8 9)))
