(ns adventoc.twentytwentyone.twentythree.amphipod
  (:require
   [clojure.core :exclude [name]])
  (:gen-class))

(def amphipod-energys {:A 1
                       :B 10
                       :C 100
                       :D 1000})

(def amphipods {::A0 {:moves []}
                ::A1 {:moves []}
                ::B0 {:moves []}
                ::B1 {:moves []}
                ::C0 {:moves []}
                ::C1 {:moves []}
                ::D0 {:moves []}
                ::D1 {:moves []}})

(defn amphipod-energy [amphipod]
  (if-let [e (some-> amphipod name first str keyword amphipod-energys)]
    e
    (throw (ex-info (str "Unknown amphipod energy for : " amphipod) {:amphipod amphipod}))))

(defn positions->moves [positions]
  (let [coords (for [row-index (range (count positions))
                     col-index (range (count (positions row-index)))]
                 [row-index, col-index])]
    (reduce (fn [acc, [x, y]]
              (let [amphipod (get-in positions [x y])]
                (if amphipod
                  (update-in acc [amphipod :moves] conj [x y])
                  acc)))
            amphipods
            coords)))

(defn -main [& _]
  (let [board [[nil, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil]
               [nil, nil, ::B0, ::C0, ::B1, ::D1, nil, nil, nil, nil, nil]
               [nil, nil, ::A0, ::D0, ::C1, ::A1, nil, nil, nil, nil, nil]]
        amphipods-start (positions->moves board)]
    (println amphipods-start)))

(comment (amphipod-energys (keyword (str (first (name ::B1))))))

;; #############
;; #...........#
;; ###B#C#B#D###
;;   #A#D#C#A#
;;   #########
