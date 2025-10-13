(ns adventoc.ancillary.tower-of-hanoi.toh)

(defn towers [disks towers]
  (let [tower-zero (vec (range disks))
        empty-tower (vec (repeat disks nil))
        initial (vec (cons tower-zero (repeat (dec towers) empty-tower)))]
    initial))

(defn toh-recursive [state, bar-one-index, bar-two-index, bar-three-index, n]
  (if (zero? n)
    state
    ;; else
    (let [state-next (toh-recursive state, bar-one-index, bar-three-index, bar-two-index, (dec n))
          disk-to-move (get-in state-next [bar-one-index (dec n)])
          state-moved (-> state-next
                          (assoc-in [bar-one-index (dec n)] nil)
                          (assoc-in [bar-three-index (dec n)] disk-to-move))]
      (toh-recursive state-moved, bar-two-index, bar-one-index, bar-three-index, (dec n)))))

(defn toh [number-of-disks, number-of-towers]
  (let [state (towers number-of-disks number-of-towers)]
    (toh-recursive state 0 1 (dec number-of-towers) number-of-disks)))

(defn -main [& _]
  (println (toh 4 3)))

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
