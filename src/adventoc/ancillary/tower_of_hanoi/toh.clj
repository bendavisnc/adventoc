(ns adventoc.ancillary.tower-of-hanoi.toh)

(defn towers [disks towers]
  (let [tower-zero (vec (range disks))
        empty-tower (vec (repeat disks nil))
        initial (vec (cons tower-zero (repeat (dec towers) empty-tower)))]
    initial))

(defn toh [number-of-disks, number-of-towers]
  (let [state (towers number-of-disks number-of-towers)]
    (println state)))

(defn -main [& _]
  (println (towers 4 3)))
