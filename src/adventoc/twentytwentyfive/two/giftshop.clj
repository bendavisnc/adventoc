(ns adventoc.twentytwentyfive.two.giftshop
  (:require
   [clojure.string :as string]))

(defn is-valid? [id]
  (if (>= 1 (count id))
    true
    (as-> id $
      [(subs id 0 (quot (count id) 2))
       (subs id (quot (count id) 2))]
      (set $)
      (count $)
      (not= 1 $))))

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

(defn giftshop [input]
  (let [ids (input->ids input)
        invalid-ids (filter (comp not is-valid?) ids)
        sum (apply + (map Long/parseLong invalid-ids))]
    sum))

