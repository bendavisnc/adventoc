(ns adventoc.twentyfifteen.four.theidealstockingstuffer
  (:import
   (java.security MessageDigest)))

;; https://gist.github.com/jinyangustc/05e97ae35f20459bd99f15ea0fc345ae

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn theidealstockingstuffer [secret-key, zeros-count]
  (some (fn [i]
          (when
            (= zeros-count
              (count (take-while #{\0}
                       (md5 (str secret-key i)))))
            i))
        (range ##Inf)))

(comment (md5 "The quick brown fox jumps over the lazy dog"))

(comment (count (take-while #{\0} "000012")))

(comment (theidealstockingstuffer "abcdef" 5))

(comment (theidealstockingstuffer "pqrstuv" 5))
