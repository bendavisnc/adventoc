(ns adventoc.twentyfifteen.one.notquitelisp)

(defn paren->num [c]
  (case c
    \( 1
    \) -1
    (throw (ex-info "Unexpected character to convert to number from parenthesis." {:c c}))))

(defn notquitelisp [input]
  (apply +
         (map paren->num input)))

(defn basement-position [input]
  (loop [input-index 1]
    (cond
      (> input-index (count input))
      (throw (ex-info "Reached end of input." {:input input
                                               :input-index input-index}))
      (= -1 (notquitelisp (take input-index input)))
      input-index
      :else
      (recur (inc input-index)))))
