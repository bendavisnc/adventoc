(ns adventoc.ancillary.tower-of-hanoi.toh
  (:require
   [dev.nu.morse :as morse]))

(defn towers [disks towers]
  (let [tower-zero (vec (range (dec disks) -1 -1))
        empty-tower []
        initial (vec (cons tower-zero (repeat (dec towers) empty-tower)))]
    initial))

;; (defn toh-moves
;;   "Returns a lazy sequence of [from to] moves to solve Hanoi for n disks."
;;   [n from to aux]
;;   (if (zero? n)
;;     []
;;     (concat
;;       ;; move n-1 disks from source → auxiliary
;;       (toh-moves (dec n) from aux to)
;;       ;; move nth disk from source → destination
;;       [[from to]]
;;       ;; move n-1 disks from auxiliary → destination
;;       (toh-moves (dec n) aux to from))))

(defn toh-moves
  ([n from to aux] (toh-moves n from to aux 0))
  ([n from to aux depth]
   (let [indent (apply str (repeat (* 2 depth) " "))]
     (println indent "toh-moves n=" n "from" from "to" to "aux" aux)
     (if (zero? n)
       []
       (concat
         (toh-moves (dec n) from aux to (inc depth))
         [[from to]]
         (toh-moves (dec n) aux to from (inc depth)))))))

(defn apply-move [state [from to]]
  (let [disk (peek (state from))]
    (-> state
        (assoc from (pop (state from)))
        (assoc to   (conj (state to) disk)))))

(defn toh [number-of-disks, number-of-towers]
  (let [state (towers number-of-disks number-of-towers)]
    (reduce apply-move state (toh-moves number-of-disks 0 2 1))))

(defn -main [& _]
;;   (println (toh 4 3)))
;;   (println (toh-moves 3 0 2 1)))
;;   (println (toh 3 3)))
  (println (toh-moves 2 0 2 1)))

(comment (morse/launch-in-proc))
(comment (-main))

(comment (toh-moves 4 0 2 1))

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
