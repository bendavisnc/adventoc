(ns adventoc.twentytwentyfive.one.secretentrance
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(def dial-max 99)

(defn dialstep->n [s]
  (let [direction (first s)
        number (Integer/parseInt (apply str (rest s)))]
    (* (case direction
         \L -1
         \R 1)
       number)))

(defn apply-dialsteps [dial, dialsteps]
  (reduce (fn [acc, step]
            (let [dialnumber (last (:numbers acc))
                  dialnumber-next (mod (+ dialnumber step)
                                       (inc dial-max))
                  mod-zeros (abs (quot (+ dialnumber step)
                                       (inc dial-max)))
                  left-of-zero (and (pos? dialnumber)
                                    (not (pos? (+ dialnumber step))))
                  zeros-add (+ mod-zeros (if left-of-zero 1 0))]
              (-> acc
                  (update :numbers conj dialnumber-next)
                  (update :zeros (fnil + 0) zeros-add))))
          dial
          dialsteps))

(defn secretentrance
  ([input {:keys [method-click?]}]
   (time (println
           (let [dialnumbers-start [50]
                 dialsteps (map dialstep->n (string/split-lines input))
                 dialsteps-applied (apply-dialsteps {:numbers dialnumbers-start} dialsteps)]
             (if method-click?
               (:zeros dialsteps-applied)
               (count (filter zero? (:numbers dialsteps-applied))))))))
  ([input]
   (secretentrance input {})))

(defn -main [& args]
  (if (= ["--method-click"] args)
    (secretentrance (input) {:method-click? true})
    (secretentrance (input))))
