(ns adventoc.helpers
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(defn input
  ([filepath]
   (slurp (io/resource filepath)))
  ([]
   (let [filepath-ns (-> (Thread/currentThread)
                         (.getStackTrace)
                         (get 3)
                         (.getClassName))
         _ (assert (< (count "adventoc.helpers")
                      (count filepath-ns))
                   (format "Use of unexpected ns, `%s`" filepath-ns))
         filepath (string/join "/" (concat (drop-last (string/split filepath-ns #"\."))
                                           ["input.txt"]))]
     (input filepath))))

