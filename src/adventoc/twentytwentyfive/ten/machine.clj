(ns adventoc.twentytwentyfive.ten.machine
  (:require
    [clojure.string :as string]))

(def 💡 true)

(def ■ false)


(def ^:private on-char \#)
(def ^:private off-char \.)

(defn- str->lights
  [s]
  (vec (for [c s]
         (cond
           (= on-char c)  true
           (= off-char c) false
           :else          (throw (new Exception
                                      (format
                                       "Unexpected on/off character, `%s`"
                                       [c (type c)])))))))

(defn- str->buttons
  [s]
  (let [buttons-regex #"(?<=\().*?(?=\))"]
    (vec (for [button-s (re-seq buttons-regex s)]
           (vec (for [int-s (string/split button-s #",")]
                  (parse-long int-s)))))))


(defn- str->joltages
  [s]
  (vec (for [int-s (string/split s #",")]
         (parse-long int-s))))


(defn str->machine
  [s]
  (let [joltages-regex   #"(?<=\{).*(?=\})"
        lights-regex     #"(?<=\[).*(?=\])"
        buttons-regex    #"(?<=\s).*(?=\s)"
        joltages-diagram (str->joltages (re-find joltages-regex s))
        lights-diagram   (str->lights (re-find lights-regex s))]
    {::joltages-diagram joltages-diagram
     ::joltages         (vec (repeat (count joltages-diagram)
                                     0))
     ::lights-diagram   lights-diagram
     ::lights           (vec (repeat (count lights-diagram)
                                     false))
     ::buttons          (str->buttons (re-find buttons-regex s))}))
