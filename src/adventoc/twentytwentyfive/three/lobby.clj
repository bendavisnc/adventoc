(ns adventoc.twentytwentyfive.three.lobby
  (:require
   [clojure.string :as string]))

(defn batterybank->joltage
  ([batterybank, joltage-length]
   (let [index-pairs (reverse (sort-by (juxt second (comp - first))
                                       (map-indexed vector
                                                    (map (comp Long/parseLong str) batterybank))))
         numbers (loop [acc []
                        index-min 0
                        index-max (- (count batterybank)
                                     joltage-length)]
                   (if (= joltage-length (count acc))
                     acc
                     (let [[i, n] (some (fn [[i, n]]
                                          (when (<= index-min i index-max)
                                            [i, n]))
                                        index-pairs)]
                       (recur (conj acc n)
                              (inc i)
                              (inc index-max)))))]
     (Long/parseLong (apply str numbers))))
  ([batterybank]
   (batterybank->joltage batterybank 2)))

(defn lobby
  ([input {:keys [joltage-length]}]
   (apply +
          (map #(batterybank->joltage % joltage-length)
               (string/split-lines input))))
  ([input]
   (lobby input {:joltage-length 2})))
