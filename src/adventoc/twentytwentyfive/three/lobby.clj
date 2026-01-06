(ns adventoc.twentytwentyfive.three.lobby
  (:require
   [adventoc.helpers :refer [input]]
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
   (time (println
           (apply +
                  (map #(batterybank->joltage % joltage-length)
                       (string/split-lines input))))))
  ([input]
   (lobby input {:joltage-length 2})))

(defn -main [& args]
  (if (= ["--joltage-length-extended"] args)
    (lobby (input) {:joltage-length 12})
    (lobby (input))))
