(ns random-access-map.core
  (:gen-class))

(defmacro if_cmp
  [value threshold less equal greater]
  `(let [val# ~value
         thresh# ~threshold
         cmp_val# (compare val# thresh#)]
     (if (< cmp_val# 0)
       ~less
       (if (< 0 cmp_val#)
         ~greater
         ~equal))))

#_(type/defalias color
  (type/U ':red ':black ':white ':double-black))
#_(type/defalias number
    (type/U java.lang.Number
            clojure.lang.BigInt
            clojure.lang.Ratio))
; ranked tree
(deftype IndexedTree [ltree
                     key
                     val
                     size
                     color
                     rtree])

; can be black or double black.
(deftype IndexedLeaf [color])

(def black-leaf (IndexedLeaf. :black))
(def double-black-leaf (IndexedLeaf. :double-black))

; Only until we use the standard ones.
(defprotocol random-access-map
  (empty? [this] "Returns whether this set is empty")
  (count [this] "Returns the number of key/value pairs in this map")
  (contains? [this key] "Detects whether the key/value pair whose key matches <key> is present in this map")
  (get [this key] "Gets the value associated with <key> in this map")
  (nth [this index] "Gets the nth key/value pair")
  (assoc [this key val] "Add a keyed element to the map"))
; This is to come.
;  (dissoc [this key] "Return a map with the key removed"))

(extend-protocol random-access-map
  IndexedLeaf
  (empty? [this] true)
  (count [this] 0)
  (contains? [this] false)
  (get [this key]
    (throw (ex-info "Cannot get a value from a leaf."
                    {:type :random-access-map/leaf/get})))
  (nth [this index]
    (throw (ex-info "Cannot get a value from a leaf."
                    {:type :random-access-map/leaf/nth})))
  (assoc [this key val]
    (IndexedTree. (IndexedLeaf. :black) key val :red 1 (IndexedLeaf. :black)))
  (dissoc [this key]
    (throw (ex-info "Cannot remove a k/v pair from a leaf."
                    {:type :random-access-map/leaf/dissoc
                     :attempt :dissoc}))))

(defn- remove-max [tree]
  (cond (empty? tree)
        (throw (ex-info "Cannot remove a k/v pair from a leaf."
                        {:type :random-access-map/leaf/dissoc
                         :attempt :remove-max}))
        (empty? (.rtree tree))
        [(.ltree tree)
         (.key tree)
         (.val tree)]
        :else
        (let [[rettree key val] (remove-max (.rtree tree))]
          [(IndexedTree. (.ltree tree)
                         (.key tree)
                         (.val tree)
                         (dec (.size tree))
                         (.color tree)
                         rettree)
           key val])))

(extend-type IndexedTree
  random-access-map
  (count [this]
    (.size this))
  (empty? [this] false)
  (contains? [this key]
    (if_cmp key (.key this)
            (contains? (.ltree this) key)
            true
            (contains? (.rtree this) key)))
  (get [this key]
    (if_cmp key (.key this)
            (get (.ltree this) key)
            (.val this)
            (get (.rtree this) key)))
  (nth [this index]
    (if (or (> index (count this)) (< index 0))
      (throw (ex-info (str "Index '" index "' is out of the range [0,"
                           (count this) ").")
                      {:type :random-access-map-rat/out-of-range}))
      (let [lsize (count (.ltree this))]
        (if_cmp lsize index
                (nth (.rtree this) (- index lsize 1))
                (.elem this)
                (nth (.ltree this) index)))))
  (assoc [this key val]
    (if_cmp key (.key this)
            (IndexedTree. (assoc (.ltree this) key val)
                          (.key this)
                          (.val this)
                          (inc (.size this))
                          (.color this)
                          (.rtree this))
            (IndexedTree. (.ltree this)
                          key
                          val
                          (.size this)
                          (.color this)
                          (.rtree this))
            (IndexedTree. (.ltree this)
                          (.key this)
                          (.val this)
                          (inc (.size this))
                          (.color this)
                          (assoc (.rtree this) key val))))
  (dissoc [this key]
    (if_cmp key (.key this)
            (IndexedTree. (dissoc (.ltree this) key val)
                          (.key this)
                          (.val this)
                          (dec (.size this))
                          (.color this)
                          (.rtree this))
            (cond (and (empty? (.ltree this))
                       (empty? (.rtree this)))
                  black-leaf
                  (empty? (.ltree this))
                  (.rtree this)
                  (empty? (.rtree this))
                  (.ltree this)
                  :else
                  (let [[rettree key val]
                        (remove-max (.ltree this))]
                    (IndexedTree. rettree
                                  key
                                  val
                                  (dec (.size this))
                                  (.color this)
                                  (.rtree this))))
            (IndexedTree. (.ltree this)
                          (.key this)
                          (.val this)
                          (dec (.size this))
                          (.color this)
                          (dissoc (.rtree this) key val)))))
