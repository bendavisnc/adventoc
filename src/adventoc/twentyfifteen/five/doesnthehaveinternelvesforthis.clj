(ns adventoc.twentyfifteen.five.doesnthehaveinternelvesforthis
  (:require
   [clojure.string :as string]))

(def outlaw-string? #{"ab", "cd", "pq", "xy"})

(def vowel? #{\a, \e, \i, \o, \u})

(defn is-nice? [s]
  (let [{:keys [outlaw-strings, vowels, letters-twice]}
        (reduce (fn [acc, i]
                  (let [c (get s i)
                        c-prior (get s (dec i))
                        cc (str c-prior c)]
                    (cond-> acc
                      (outlaw-string? cc)
                      (update :outlaw-strings (fnil inc 0))
                      (vowel? c)
                      (update :vowels (fnil inc 0))
                      (= c c-prior)
                      (update :letters-twice (fnil inc 0)))))
                {}
                (range (count s)))]
    (and (not outlaw-strings)
         vowels
         (>= vowels 3)
         letters-twice
         (pos-int? letters-twice))))

(defn is-nice-less-ridiculous? [s]
  (let [{:keys [letters-repeat, letters-pair]}
        (reduce (fn [acc, i]
                  (let [c (get s i)
                        c-prior (get s (dec i))
                        cc (str c-prior c)]
                    (cond-> acc
                      (and (= 2 (count cc))
                           (not= -1
                                 (.indexOf (subs s
                                                 (min (count s)
                                                      (inc i)))
                                           cc)))
                      (update :letters-pair (fnil inc 0))
                      (and c
                           (= c (get s (+ i 2))))
                      (update :letters-repeat (fnil inc 0)))))
                {}
                (range (count s)))]
    (boolean (and letters-pair letters-repeat))))

(defn doesnthehaveinternelvesforthis
  ([input {:keys [less-ridiculous?]}]
   (let [is-nice?' (if less-ridiculous? is-nice-less-ridiculous? is-nice?)]
     (->> input
          string/split-lines
          (filter is-nice?')
          count)))
  ([input]
   (doesnthehaveinternelvesforthis input {})))

