(ns ranked-set.core)

(defprotocol RankedSet
  "Retrieve elements by the rank of their magnitude."
  (add [rset value magnitude] "Add a value and its magnitude to the set.")
  (retrieve [rset rank] "Retrieve value by its magnitude's rank.")
  (cut [rset rank] "Remove a value by its magnitude's rank.")
  (size [rset] "Get the number of values stored in the set."))

(extend-protocol RankedSet
  nil
  (add [rset value magnitude] nil)
  (retrieve [rset rank] nil)
  (cut [rset rank] nil)
  (size [rset] 0))
   ;; get contains disjoin count cons empty equiv seq count

(defrecord RBTreeRankedSet [left right value magnitude children])
(extend-protocol RankedSet
  RBTreeRankedSet
  (add [rset value magnitude]
    (let [left (.left rset)
          right (.right rset)
          value (.value rset)
          magnitude (.magnitude rset)
          children (.children rset)]
      (cond (nil? value)
              (RBTreeRankedSet. left right value magnitude (inc children))
            :else nil)))
  (retrieve [rset rank])
  (cut [rset rank])
  (size [rset]))

(defn ranked-set []
  (RBTreeRankedSet. nil nil nil nil 0))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
