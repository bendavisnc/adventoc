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

(defn fresh-ids-count [ranges, ids]
  (let [range-fns (map (partial apply is-in-range?) ranges)
        in-any-range? (apply some-fn range-fns)]
    (count (filter in-any-range? ids))))

(defn ranges-no-intersections [ranges]
  (reverse (reduce (fn [acc, [start, end]]
                     (let [[prior-start, prior-end] (some-> acc first)]
                       (if (<= start (or prior-end ##-Inf))
                         (conj (rest acc)
                               (list prior-start (max prior-end, end)))
                         (conj acc (list start end)))))
             nil
             (sort-by first ranges))))

(defn fresh-ids-range-sum [ranges]
  (reduce (fn [acc [start, end]]
            (+ acc
               (- end start)
               1))
          0
          (ranges-no-intersections ranges)))

(defn cafeteria
  ([input {:keys [all-ids?]}]
   (time (println
           (let [[ranges, ids] (input->fresh-ingrediant-ranges-and-ids input)]
             (if all-ids?
               (fresh-ids-range-sum ranges)
               (fresh-ids-count ranges ids))))))
  ([input]
   (cafeteria input {})))

(defn -main [& args]
  (if (= ["--all-ids"] args)
    (cafeteria (input) {:all-ids? true})
    (cafeteria (input))))
