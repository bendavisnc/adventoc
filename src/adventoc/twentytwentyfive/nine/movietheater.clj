(ns adventoc.twentytwentyfive.nine.movietheater
  (:require
    [adventoc.helpers :refer [input]]
    [clojure.set :as set]
    [clojure.string :as string]))

(def red-tile "🟥")

(def green-tile "🟩")

(def void-tile "⬛")

(def outside-tile "⬜")

(def chosen-tile "🟪")

(defn input->coords
  "Returns a list of vectors of size two that represent an x y coordinate."
  [input]
  (vec (for [line (string/split-lines input)]
         (vec (for [cell (string/split line #",")
                    :let [coord (parse-long cell)]]
                coord)))))

(defn empty-grid
  "Returns a vector of vectors that represent the structure of a grid with `height` rows of `width` length."
  [{:keys [width height]}]
  (vec (for [_ (range height)]
         (vec (repeat width nil)))))

(defn all-coord-combos
  "Returns a list of all possible pairings from 2 of the same set of a range of 0 to `n` indices."
  [n]
  (for [i (range n)
        j (range i)]
    [i j]))

(defn area
  [[x1 y1] [x2 y2]]
  (* (inc (abs (- x2 x1)))
     (inc (abs (- y2 y1)))))

(defn coords-by-area
  "Returns a list of vectors of size 2 where the first item is 2 coordinates that represent a rectangle's diagonal and the second item represents the corresponding area. 
   Returned area is sorted by area, descending."
  [coords coord-combos uncompress]
  (->> (for [[idx-a idx-b] coord-combos
             :let          [[x1 y1] (nth coords idx-a)
                            [x2 y2] (nth coords idx-b)
                            a       (area (uncompress [x1 y1])
                                          (uncompress [x2 y2]))]
             :when         (not (or (= x1 x2)
                                    (= y1 y2)))]
         [[[x1 y1]
           [x2 y2]]
          a])
       (sort-by (comp #(* -1 %)
                      second))))

(defn all-corners
  [[x1 y1] [x2 y2]]
  (let [x1' (min x1 x2)
        y1' (min y1 y2)
        dx  (abs (- x2 x1))
        dy  (abs (- y2 y1))]
    [[x1' y1'] [(+ x1' dx) y1']
     [(+ x1' dx) (+ y1' dy)] [x1' (+ y1' dy)]]))

(defn coords->connected-set
  "Returns a set of all the boundary coords determined by `coords`. 
   Assumes every successive coord has an equal x or y value."
  [coords]
  (loop [acc     #{}
         a-to-bs (concat (partition 2 1 coords)
                         [[(last coords) (first coords)]])]
    (if (empty? a-to-bs)
      acc
      (let [[[[x1 y1] [x2 y2]] & rest-a-to-bs] a-to-bs]
        (if (= x1 x2)
          (let [[start-y end-y] (sort [y1 y2])]
            (recur (into acc
                         (for [y (range start-y (inc end-y))]
                           [x1 y]))
                   rest-a-to-bs))
          (if (= y1 y2)
            (let [[start-x end-x] (sort [x1 x2])]
              (recur (into acc
                           (for [x (range start-x (inc end-x))]
                             [x y1]))
                     rest-a-to-bs))
            (throw (new Exception (str ["bad state" [[x1 y1] [x2 y2]]])))))))))

(defn ->outside-set
  "`flood fills` every coord that is not within the bounded area determined by `is-boundary?`."
  [grid-width grid-height is-boundary?]
  (let [dirs [[-1 0] [1 0] [0 -1] [0 1]]]
    (assert (not (is-boundary? [0 0]))
            "Bad assumption, top left is included in boundary from input.")
    (loop [frontier (list [0 0])
           visited  #{}
           outside  #{}]
      (if-let [[pos & more] (seq frontier)]
        (let [[x y] pos]
          (if (or (visited pos)
                  (neg-int? x)
                  (neg-int? y)
                  (>= x grid-width)
                  (>= y grid-height)
                  (is-boundary? pos))
            (recur more visited outside)
            (recur (concat more
                           (for [[dx dy] dirs]
                             [(+ x dx) (+ y dy)]))
                   (conj visited pos)
                   (conj outside pos))))
        outside))))

(defn compressed-coords
  "Transforms a list of possibly sparse coords to ones that that have the smallest gap between x/y mins and maxes.
   Smallest coordinate number is 1. Also returns a function for transforming back a coordinate."
  [coords]
  (let [unique-x   (sort (seq (set (map first coords))))
        x->i       (into {}
                         (map-indexed (fn [i x] [x (inc i)])
                                      unique-x))
        i->x       (set/map-invert x->i)
        unique-y   (sort (seq (set (map second coords))))
        y->i       (into {}
                         (map-indexed (fn [i y] [y (inc i)])
                                      unique-y))
        i->y       (set/map-invert y->i)
        compressed (mapv (fn [[x y]]
                           [(x->i x)
                            (y->i y)])
                         coords)
        uncompress (fn [[x y]]
                     [(i->x x)
                      (i->y y)])]
    {:coords     compressed
     :uncompress uncompress}))

(defn grid-with-tiles
  [grid coord->tile]
  (let [grid-width  (dec (count (first grid)))
        grid-height (dec (count grid))
        insert      (fn [grid' [x y]]
                      (assoc-in grid'
                       [y x]
                       (or (coord->tile [x y]) void-tile)))]
    (loop [grid' grid
           x     0
           y     0]
      ;; (prn [:waaat x y (is-tile? [x y])])
      (if (and (>= x grid-width)
               (>= y grid-height))
        (insert grid' [x y])
        (if (>= x grid-width)
          (recur (insert grid' [x y])
                 0
                 (inc y))
          (recur (insert grid' [x y])
                 (inc x)
                 y))))))

(defn grid->str
  [grid]
  (string/join "\n"
               (map string/join
                    grid)))

(defn movietheater
  ([input {:keys [compress? respect-boundary? skip-grid-print?]}]
   (let [{:keys [coords uncompress]} (when compress?
                                       (compressed-coords (input->coords
                                                           input)))
         coords (if compress? coords (input->coords input))
         uncompress (if compress? uncompress identity)
         max-x (inc (inc (apply max (map first coords))))
         max-y (inc (inc (apply max (map second coords))))
         is-red-tile? (set coords)
         is-red-or-green-tile? (coords->connected-set coords)
         is-outside-tile? (->outside-set max-x max-y is-red-or-green-tile?)
         coord-combos (all-coord-combos (count coords))
         coords-by-area' (coords-by-area coords coord-combos uncompress)
         [found-pair-compressed found-area]
         (some (fn [[coord-pair pair-area]]
                 (when (every?
                        (fn [corner-coord]
                          (or (not (true? respect-boundary?))
                              (not (is-outside-tile? corner-coord))))
                        (coords->connected-set
                         (apply all-corners coord-pair)))
                   [coord-pair pair-area]))
               coords-by-area')
         found-pair (map uncompress found-pair-compressed)
         grid (empty-grid {:width max-x :height max-y})
         is-chosen-tile? (set (coords->connected-set
                               (apply all-corners found-pair-compressed)))
         coord->tile (fn [coord]
                       (cond (is-chosen-tile? coord)       chosen-tile
                             (is-red-tile? coord)          red-tile
                             (is-red-or-green-tile? coord) green-tile
                             (is-outside-tile? coord)      outside-tile))

         grid' (when (not (true? skip-grid-print?))
                 (grid-with-tiles grid coord->tile))]
     {:grid   (some-> grid'
                      grid->str)
      :coords found-pair
      :area   found-area}))
  ([input]
   (movietheater input {})))

(defn -main
  [& args]
  (time
   (let [compress?        (some->> (first args)
                                   (re-matches
                                    #".*(--compress).*")
                                   second
                                   boolean)
         respect-boundary? (some->> (first args)
                                    (re-matches
                                     #".*(--respect-boundary).*")
                                    second
                                    boolean)
         skip-grid-print? (some->> (first args)
                                   (re-matches
                                    #".*(--skip-grid-print).*")
                                   second
                                   boolean)

         result           (movietheater (input)
                                        {:compress?         compress?
                                         :respect-boundary? respect-boundary?
                                         :skip-grid-print?  skip-grid-print?})]
     (println (:grid result))
     (println (:area result)))))
