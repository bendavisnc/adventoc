(ns adventoc.twentyfifteen.one.notquitelisp)

(defn paren->num [c]
  (case c
    \( 1
    \) -1
    (throw (ex-info "Unexpected character to convert to number from parenthesis." {:c c}))))

(defn notquitelisp [input]
  (apply +
         (map paren->num input)))
