(ns adventoc.twentytwentyfive.three.lobby
  (:require
   [clojure.string :as string]))

(defn batterybank->joltage [batterybank]
  (let [index-pairs (reverse (sort-by (juxt second (comp - first))
                                      (map-indexed vector
                                                   (map (comp Long/parseLong str) batterybank))))
        [greatest-index, greatest] (some (fn [[i, n]]
                                           (when (not= i (dec (count batterybank)))
                                             [i, n]))
                                         index-pairs)
        greatest-next (some (fn [[i, n]]
                              (when (> i greatest-index)
                                n))
                            index-pairs)]
    (Long/parseLong (str greatest greatest-next))))

(defn lobby [input]
  (apply +
         (map batterybank->joltage
              (string/split-lines input))))
