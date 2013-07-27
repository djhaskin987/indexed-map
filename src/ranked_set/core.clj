(ns ranked-set.core)

(defprotocol ranked-set
  "Retrieve elements by the rank of their magnitude."
  (add [rset value magnitude] "Add a value and its magnitude to the set.")
  (retrieve [rset rank] "Retrieve value by its magnitude's rank.")
  (cut [rset rank] "Remove a value by its magnitude's rank.")
  (size [rset] "Get the number of values stored in the set."))

(extend-protocol ranked-set
  nil
  (add [rset value magnitude] nil)
  (retrieve [rset rank] nil)
  (cut [rset rank] nil)
  (size [rset] 0))
   ;; get contains disjoin count cons empty equiv seq count

(deftype rbtree-ranked-set [left value magnitude children right]
  ranked-set
  (add [rset value magnitude]
    )
  (retrieve [rset rank])
  (cut [rset rank])
  (size [rset]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
