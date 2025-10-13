(ns adventoc.ancillary.tower-of-hanoi.toh
  (:require
   [dev.nu.morse :as morse]))

(defn towers [disks towers]
  (let [tower-zero (into clojure.lang.PersistentQueue/EMPTY (range (dec disks) -1 -1))
        empty-tower clojure.lang.PersistentQueue/EMPTY
        initial (vec (cons tower-zero (repeat (dec towers) empty-tower)))]
    initial))

(defn switch-towers [state, a-index, b-index]
  (let [a (get state a-index)
        b (get state b-index)]
    (-> state
        (assoc a-index b)
        (assoc b-index a))))

(defn move-disk [state]
  (if-let [disk-to-move (-> state first peek)]
    (-> state
        (update-in [0] pop)
        (update-in [2] conj disk-to-move))
    state))

(defn print-state [state]
  (println (mapv vec state)))

(defn toh-recursive [state, n]
  (if (zero? n)
    (do
    ;;   (println state)
      (morse/inspect [n state])
      state)
    ;; else
    (let [state-next (toh-recursive (switch-towers state 0 1) (dec n))
          state-moved (move-disk state-next)]
      (toh-recursive (switch-towers state-moved 0 1) (dec n)))))

(defn toh [number-of-disks, number-of-towers]
  (let [state (towers number-of-disks number-of-towers)]
    (toh-recursive state number-of-disks)))

(defn -main [& _]
  (let [solved (mapv vec (toh 4 3))]
    (println "Final state: " solved)
    (assert (= [] (first solved)))
    (assert (= [] (second solved)))
    (assert (= [3, 2, 1, 0] (nth solved 2)))))

(comment (morse/launch-in-proc))
(comment (-main))

(comment (let [disk [0, 1, 2, 3 4]]
           (println (pop disk))))
(comment (println (range 4 -1 -1)))

(comment (conj [] 1))

;; https://www.reddit.com/r/learnprogramming/comments/lywzqh/at_what_point_of_learning_programming_am_i/
    ;; void f(int[] bar1, int[] bar2, int[] bar3, int n)
    ;;     if(n == 0)
    ;;         return;
    ;;     // move n-1 disks from bar1 to bar2
    ;;     f(bar1, bar3, bar2, n-1);
    ;;     // move the last disk from bar1 to bar3
    ;;     bar3[n] = bar1[n];
    ;;     bar1[n] = 0;
    ;;     // Move the remaining n-1 disks from bar2 to bar3
    ;;     f(bar2, bar1, bar3, n-1);
