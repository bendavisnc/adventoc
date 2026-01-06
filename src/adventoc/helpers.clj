(ns adventoc.helpers
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn input []
  (let [filepath (-> (Thread/currentThread)
                     (.getStackTrace)
                     (get 3)
                     (.getClassName))]
    (assert (< (count "adventoc.helpers")
               (count filepath))
            (format "Use of unexpected ns, `%s`" filepath))
    (slurp (io/resource (string/join "/" (concat (drop-last (string/split filepath #"\."))
                                           ["input.txt"]))))))
