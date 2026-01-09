(ns adventoc.twentytwentyfive.five.cafeteria
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(defn is-in-range? [start, end]
  (fn [i]
    (and (<= start i)
         (>= end i))))

(defn input->fresh-ingrediant-ranges-and-ids [input]
  (let [[s1, s2] (string/split input #"\s\n")
        ranges (map (fn [s]
                      (map parse-long
                        (string/split s #"-")))
                    (string/split s1 #"\n"))
        ids (map parse-long (string/split s2 #"\n"))]
    [ranges, ids]))

(defn cafeteria [input]
  (let [[ranges, ids] (input->fresh-ingrediant-ranges-and-ids input)
        range-fns (map (partial apply is-in-range?) ranges)
        in-any-range? (apply some-fn range-fns)]
    (count (filter in-any-range? ids))))

(defn -main [& args]
  (time (println (cafeteria (input)))))
