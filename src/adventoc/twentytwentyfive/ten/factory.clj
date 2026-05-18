(ns adventoc.twentytwentyfive.ten.factory
  (:require
    [adventoc.twentytwentyfive.ten.machine :as m]
    [adventoc.helpers :refer [input]]
    [clojure.math.combinatorics :as combo]
    [clojure.string :as string]))

(defn input->machines
  [input]
  (for [line (string/split-lines input)]
    (m/str->machine line)))

(defn machine->increments-by-joltage-index-seq
  [machine]
  (map (fn [button]
         (map (fn [i]
                (if ((set button) i) 1 0))
              (range (count (::m/joltages machine)))))
       (::m/buttons machine)))

(defn costs-by-parity
  [increments-seq]
  (let [buttons-count  (count increments-seq)
        joltages-count (count (first increments-seq))
        all-parities   (combo/selections [0 1] joltages-count)]
    (reduce
     (fn [acc buttons-pressed-count]
       (reduce
        (fn [acc' button-indexes]
          (let [increments-acc (apply map
                                      +
                                      (conj (map (fn [button-index]
                                                   (nth increments-seq
                                                        button-index))
                                                 button-indexes)
                                            (repeat joltages-count 0)))
                increments-acc-parity-match (map #(mod % 2) increments-acc)]
            (update-in acc'
                       [increments-acc-parity-match increments-acc]
                       (fn [cost-orig]
                         (or cost-orig buttons-pressed-count)))))
        acc
        (combo/combinations (range buttons-count) buttons-pressed-count)))
     (into {} (map vector all-parities (repeat {})))
     (range (inc buttons-count)))))


(defn least-button-presses-joltages'
  [costs joltages-target]
  (if (= (set [0])
         (set joltages-target))
    0
    (let [joltages-target-parity  (map #(mod % 2) joltages-target)
          costs-at-joltage-parity (filter (fn [[incs _]]
                                            (every?
                                             (fn [[a b]] (<= a b))
                                             (map vector incs joltages-target)))
                                          (costs joltages-target-parity))]
      (if (not (seq costs-at-joltage-parity))
        ##Inf
        (apply min
               (map (fn [[incs cost]]
                      (let [joltages-target' (map (fn [a b]
                                                    (quot (- a b)
                                                          2))
                                                  joltages-target
                                                  incs)]
                        (+ cost
                           (* 2
                              (least-button-presses-joltages'
                               costs
                               joltages-target')))))
                    costs-at-joltage-parity))))))

(defn least-button-presses-joltages
  [increments-seq joltages-diagram]
  (let [costs (costs-by-parity increments-seq)]
    (least-button-presses-joltages' costs joltages-diagram)))

(defn machine->least-button-presses-joltages
  [machine]
  (least-button-presses-joltages (machine->increments-by-joltage-index-seq
                                  machine)
                                 (::m/joltages-diagram
                                  machine)))

(defn machine->least-button-presses-lights
  [machine]
  (let [costs (costs-by-parity (machine->increments-by-joltage-index-seq
                                machine))
        parity-by-lights (map {false 0 true 1} (::m/lights-diagram machine))]
    (apply min (vals (costs parity-by-lights)))))

(defn factory
  [input {:keys [lights? joltages?] :as config}]
  (let [machine->least-button-presses
        (case (map boolean [lights? joltages?])
          [true false] machine->least-button-presses-lights
          [false true] machine->least-button-presses-joltages
          (throw (new Exception
                      (format "Given bad config params `%s`" config))))
        machines (input->machines input)]
    (assert (seq machines))
    (apply +
           (map #(machine->least-button-presses %)
                machines))))

(defn -main
  [& args]
  (time
   (let [lights?   (some->> (first args)
                            (re-matches
                             #".*(--lights).*")
                            second
                            boolean)
         joltages? (some->> (first args)
                            (re-matches
                             #".*(--joltages).*")
                            second
                            boolean)

         result    (factory (input)
                            {:lights?   lights?
                             :joltages? joltages?})]
     (println result))))
