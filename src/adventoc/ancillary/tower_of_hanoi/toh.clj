(ns adventoc.ancillary.tower-of-hanoi.toh
  (:require
   [dev.nu.morse :as morse]))

(defn towers [disks towers]
  (let [tower-zero (vec (range (dec disks) -1 -1))
        empty-tower []
        initial (vec (cons tower-zero (repeat (dec towers) empty-tower)))]
    initial))

(defn toh-recursive [state, bar-one-index, bar-two-index, bar-three-index, n]
  (if (zero? n)
    (do
      (println state)
      ;;   (morse/inspect state)
      state)
    ;; else
    (let [state-next (toh-recursive state, bar-one-index, bar-three-index, bar-two-index, (dec n))
          disk-to-move (-> state-next (get bar-one-index) peek)
          tower-one-after-move (-> state-next (get bar-one-index) pop)
          tower-three-after-move (-> state-next (get bar-three-index) (conj disk-to-move))
          state-moved (-> state-next
                          (assoc bar-one-index tower-one-after-move)
                          (assoc bar-three-index tower-three-after-move))]
      (toh-recursive state-moved, bar-two-index, bar-one-index, bar-three-index, (dec n)))))

(defn toh [number-of-disks, number-of-towers]
  (let [state (towers number-of-disks number-of-towers)]
    (toh-recursive state 0 1 (dec number-of-towers) number-of-disks)))

(defn -main [& _]
  (println (toh 4 3)))

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
