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

(defn splintering-exploration [grid]
  (loop [st8 {}
         rows grid]
    (cond (empty? rows)
          st8
          (empty? st8)
          (recur (assoc st8
                        (index-of (first rows) s)
                        1)
                 (rest rows))
          :else
          (let [row (first rows)]
            (recur (reduce (fn [s, i]
                             (if (= splinter (nth row i))
                               (-> s
                                   (update (dec i) (fnil + 0) (st8 i))
                                   (update (inc i) (fnil + 0) (st8 i)))
                               (update s i (fnil + 0) (st8 i))))
                           {}
                           (keys st8))
                   (rest rows))))))

(defn laboratories
  ([input {:keys [quantum]}]
   (let [grid (input->grid input)]
     (if quantum
       (apply + (vals (splintering-exploration grid)))
       (beam-splinters-count (insert-beams grid)))))
  ([input]
   (laboratories input {})))

(defn -main [& args]
  (time (println
          (if (= ["--quantum"] args)
            (laboratories (input) {:quantum true})
            (laboratories (input))))))
