(ns ranked-set.core)

(defmacro if_cmp_ascending
  [value threshold less equal greater]
  `(let [val# ~value
        thresh# ~threshold]
    (if (< val# thresh#)
      ~less
      (if (< thresh# val#)
        ~greater
        ~equal))))

(defmacro if_cmp_descending
  [value threshold greater equal less]
  `(let [val# ~value
        thresh# ~threshold]
    (if (< val# thresh#)
      ~less
      (if (< thresh# val#)
        ~greater
        ~equal))))

(defrecord RankedNode [Left Payload Magnitude RankSum Right])

(defn empty-ranked-set
  "Creates an empty ranked set." []
  (->RankedNode nil nil nil nil nil))
(defn ranked-set-add
  "Adds to a ranked set."
  [rnode element magnitude]
  (let [{Left :Left Payload :Payload RankSum :RankSum
         Magnitude :Magnitude Right :Right}
        rnode]
  ; If the magnitude is nil, the node is assumed to be the empty set!
  ; This is reasonable because the magnitude must be comparable, and nil is not.
  (if (nil? Magnitude)
    (->RankedNode Left element magnitude 1 Right)
    (if_cmp_descending magnitude Magnitude
            (->RankedNode
              (ranked-set-add Left element magnitude)
              Payload
              Magnitude
              (inc RankSum)
              Right)
            ;; If the Magnitudes are equal,
            ;; Don't do anything
            (->RankedNode
              Left
              Payload
              Magnitude
              RankSum
              Right)
            (->RankedNode
              Left
              Payload
              Magnitude
              RankSum
              (ranked-set-add Right element Magnitude))))))
