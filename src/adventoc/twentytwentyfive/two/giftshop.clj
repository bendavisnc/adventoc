(ns adventoc.twentytwentyfive.two.giftshop
  (:require
   [adventoc.helpers :refer [input]]
   [clojure.string :as string]))

(defn is-valid? [id]
  (assert (<= 2 (count id)) "Expected id to be longer than a character.")
  (->
    [(subs id 0 (quot (count id) 2))
     (subs id (quot (count id) 2))]
    set
    count
    (not= 1)))

(defn multiples [n]
  (for [i (range 1 (inc (quot n 2)))
        :when (zero? (mod n i))]
    i))

(defn is-valid-loosened-edition? [id]
  (boolean (not (some (fn [n]
                        (->> id
                             (partition n)
                             set
                             count
                             (= 1)))
                      (multiples (count id))))))

(defn input->ids [input]
  (let [ranges-s (string/split input #",")
        ranges (map (fn [s]
                      (as-> s s*
                        (string/split s* #"-")
                        [(first s*), (last s*)]
                        (mapv Long/parseLong s*)))
                    ranges-s)
        ids-n (mapcat (fn [[s, e]]
                        (range s (inc e)))
                      ranges)
        ids (map str ids-n)]
    ids))

(defn giftshop
  ([input {:keys [atleast-two?]}]
   (time (println
           (let [ids (input->ids input)
                 is-valid?' (if atleast-two? is-valid-loosened-edition? is-valid?)
                 invalid-ids (filter (comp not is-valid?')
                                     ids)
                 sum (apply + (map Long/parseLong invalid-ids))]
             sum))))

  ([input]
   (giftshop input {:atleast-two? false})))

(defn -main [& args]
  (if (= ["--atleast-two"] args)
    (giftshop (input) {:atleast-two? true})
    (giftshop (input))))
