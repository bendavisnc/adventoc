(ns adventoc.twentytwentyfive.ten.factory
  (:require
    [adventoc.helpers :refer [input]]
    [clojure.string :as string]
    [clojure.math.combinatorics :as combo]))

(def on-char \#)
(def off-char \.)

(defn lights->str
  [lights]
  (string/join (concat
                [\[]
                (for [light lights]
                  (cond
                    (= true light)  on-char
                    (= false light) off-char
                    :else           (throw (new Exception
                                                (format
                                                 "Unexpected on/off value, `%s`"
                                                 [light (type light)])))))
                [\]])))


(defn str->lights
  [s]
  (vec (for [c s]
         (cond
           (= on-char c)  true
           (= off-char c) false
           :else          (throw (new Exception
                                      (format
                                       "Unexpected on/off character, `%s`"
                                       [c (type c)])))))))

(defn str->buttons
  [s]
  (let [buttons-regex #"(?<=\().*?(?=\))"]
    (vec (for [button-s (re-seq buttons-regex s)]
           (vec (for [int-s (string/split button-s #",")]
                  (parse-long int-s)))))))

(defn str->machine
  [s]
  (let [lights-regex   #"(?<=\[).*(?=\])"
        buttons-regex  #"(?<=\s).*(?=\s)"
        lights-diagram (str->lights (re-find lights-regex s))]
    {::lights-diagram lights-diagram
     ::lights         (vec (repeat (count lights-diagram)
                                   false))
     ::buttons        (str->buttons (re-find buttons-regex s))}))

(defn input->machines
  [input]
  (for [line (string/split-lines input)]
    (str->machine line)))

(defn light-toggle
  [is-on?]
  (case is-on?
    true  false
    false true
    (throw (new Exception
                (format
                 "Unexpected `is-on?` value, `%s`"
                 [is-on? (type is-on?)])))))

(defn push-button
  [machine button]
  (reduce (fn [m idx]
            (update-in m [::lights idx] light-toggle))
          machine
          button))


(defn push-buttons
  [machine buttons]
  (reduce push-button
          machine
          buttons))

(defn find-least-buttons-solution
  [machine]
  (let [attempts (mapcat
                  (fn [i]
                    (map (fn [button-indexes]
                           {:button-indexes button-indexes
                            :machine        (push-buttons machine
                                                          (map
                                                           (fn [index]
                                                             (get-in machine
                                                                     [::buttons
                                                                      index]))
                                                           button-indexes))})
                         (combo/combinations (range (count (::buttons machine)))
                                             i)))
                  (iterate inc 1))
        found    (filter (fn [{:keys [machine]}]
                           (= (::lights machine)
                              (::lights-diagram machine)))
                         attempts)]
    (first found)))

(defn factory
  [input]
  (apply +
         (for [machine (input->machines input)
               :let    [{:keys [button-indexes]} (find-least-buttons-solution
                                                  machine)]]
           (count button-indexes))))

(defn -main
  [& args]
  (time (println (factory (input)))))
