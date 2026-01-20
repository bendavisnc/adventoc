(ns adventoc.twentytwentyfive.seven.laboratories
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(def s \S)
(def beam \|)
(def splinter \^)
(def vacancy \.)

(def index-of
  (fn [^String s c-or-s]
    (as-> (string/index-of s c-or-s) idx
      (when (not= idx -1) idx))))

(defn input->rows [input]
  ;; (map seq (string/split-lines input)))
  (string/split-lines input))

;; (defn laboratories [input]
;;   (dec (count (filter (fn [c]
;;                         (= splinter c))
;;                 input))))

(defn insert-beams [rows]
  (loop [acc nil
         [headrow, & restrows] rows]
    ;; have we exhausted rows to update? 
    (if-not headrow
      (reverse acc)
      ;; else
      ;; are we at the start?
      (if-let [s-index (index-of headrow s)]
        (let [[headrow' & restrows'] restrows
              firstbeamrow (apply str (map (fn [[i c]]
                                             (if (= s-index i)
                                               beam
                                               c))
                                        (map-indexed vector headrow')))]
          (recur (concat (list firstbeamrow headrow)
                         acc)
                 restrows'))

        ;; is there a left sided beam to insert?
        (if-let [left-splinter-index (index-of headrow (str vacancy splinter))]
          (let [updatedrow (apply str (map (fn [[i c]]
                                             (if (= left-splinter-index i)
                                               beam
                                               c))
                                           (map-indexed vector headrow)))]
            (recur acc
                   (conj restrows updatedrow)))

          ;; is there a right sided beam to insert?
          (if-let [right-splinter-index (index-of headrow (str splinter vacancy))]
            (let [updatedrow (apply str (map (fn [[i c]]
                                               (if (= (inc right-splinter-index) i)
                                                 beam
                                                 c))
                                          (map-indexed vector headrow)))]
              (recur acc
                (conj restrows updatedrow)))

            ;; is there a bottom beam to insert?
            (if-let [missing-beam-index (when-let [top-row (first acc)]
                                          (some (fn [[i c]]
                                                  (when (= [beam vacancy] [(nth top-row i), c])
                                                    i))
                                                (map-indexed vector headrow)))]
              (let [updatedrow (apply str (map (fn [[i c]]
                                                 (if (= missing-beam-index i)
                                                   beam
                                                   c))
                                               (map-indexed vector headrow)))]
                (recur acc
                  (conj restrows updatedrow)))
              ;; else
              (recur (conj acc headrow)
                     restrows))))))))

(defn beam-splinters-count [rows]
  (loop [how-many 0
         [headrow & restrows] rows
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

(defn laboratories [input]
  (let [rows (input->rows input)
        rowsupdated (insert-beams rows)]
    (beam-splinters-count rowsupdated)))

(defn -main [& args]
  (time (println
          (laboratories (input)))))
