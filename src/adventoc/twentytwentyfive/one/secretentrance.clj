(ns adventoc.twentytwentyfive.one.secretentrance
  (:require
   [clojure.string :as string]))

(def dial-max 99)

(defn dialstep->n [s]
  (let [direction (first s)
        number (Integer/parseInt (apply str (rest s)))]
    (* (case direction
         \L -1
         \R 1)
       number)))

(defn apply-dialsteps [dialnumbers, dialsteps]
  (reduce (fn [acc, step]
            (let [dialnumber (last acc)
                  dialnumber-next (mod (+ dialnumber step)
                                       (inc dial-max))]
              (conj acc dialnumber-next)))
          dialnumbers
          dialsteps))

(defn secretentrance [input]
  (let [dialnumbers-start [50]
        dialsteps (map dialstep->n (string/split-lines input))]
    (count (filter zero?
                   (apply-dialsteps dialnumbers-start dialsteps)))))
