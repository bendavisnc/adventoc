(ns adventoc.twentyfifteen.two.iwastoldtherewouldbenomath
  (:require
   [clojure.string :as str]))

(defn box-surface-area [length, width, height]
  (+ (* 2 length width)
     (* 2 width height)
     (* 2 height length)))

(defn iwastoldtherewouldbenomath [length, width, height]
  (+ (box-surface-area length, width, height)
     (apply * (take 2 (sort [length, width, height])))))

(defn feet-of-ribbon [length, width, height]
  (+ (* 2
        (apply + (first (sort-by (partial apply *)
                                 [[length, width], [width, height], [height, length]]))))
     (* length, width, height)))
