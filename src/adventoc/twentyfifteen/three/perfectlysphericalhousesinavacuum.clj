(ns adventoc.twentyfifteen.three.perfectlysphericalhousesinavacuum)

(defn arrow->coord [c]
  (case c
    \< [-1, 0]
    \> [1, 0]
    \^ [0, 1]
    \v [0, -1]
    (throw (ex-info "Unexpected character to convert to coord." {:c c}))))

(defn perfectlysphericalhousesinavacuum-solve [input]
  (first (reduce (fn [[acc, [xa, ya]], [xb, yb]]
                   [(update acc [xa, ya] (fnil inc 0))
                    [(+ xa xb), (+ ya yb)]])
                 [{}, [0, 0]]
                 (concat (map arrow->coord input) [[0, 0]]))))

(defn perfectlysphericalhousesinavacuum
  ([input {:keys [robo-santa?]}]
   (count
     (if robo-santa?
       (merge-with + (perfectlysphericalhousesinavacuum-solve (flatten (partition 1 2 input)))
                     (perfectlysphericalhousesinavacuum-solve (flatten (partition 1 2 (rest input)))))
       (perfectlysphericalhousesinavacuum-solve input))))
  ([input] (perfectlysphericalhousesinavacuum input nil)))
