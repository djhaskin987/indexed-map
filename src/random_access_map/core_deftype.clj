(in-ns 'random-access-map.core)

(deftype RandomAccessMap [^java.util.Comparator cmp tree]
  clojure.lang.IPersistentMap
  ; make sure this element doesn't exist.
  (without [this key]
    (if (nil? (ram-find tree key cmp))
      this
      (RandomAccessMap. cmp (remove-val tree key (fn [t] (throw (ex-info "Tried to remove a value that didn't exist."
                                                   {:type :RandomAccessMap/without
                                                    :key key})))
                  cmp))))
  ; insert with replacement.
  (assoc [this key val]
    (RandomAccessMap. cmp (insert-val tree key val (fn [t]
                                                     (let [[l k v s r] t]
                                                       [l key val s r])) cmp)))
  (assocEx [this key val]
    (throw (java.lang.UnsupportedOperationException. "Not supported")))
  ; says it.
  (iterator [this] (throw (java.lang.UnsupportedOperationException. "Iterators Not supported")))
  ; says it.
  (containsKey [this key]
    (not (nil? (ram-find tree key cmp))))
  ; vector of [k v], returns nil if it doesn't find it
  (entryAt [this key]
    (ram-find tree key cmp))
  ; get size.
  (count [this]
    (size tree))
  ; get empty collection (nil collection).
  (empty [this]
    (RandomAccessMap. cmp :black-leaf))
  ; takes a [k v] and inserts it.
  (cons [this kvpair]
    (if (nil? kvpair)
      this
      (let [[k v] kvpair]
        (assoc this k v))))
  ; equals?
  (equiv [this other]
    (and (instance? RandomAccessMap other)
         (= tree (.tree other))))
  ; get the seq!
  (seq [this]
    (if (ram-empty? tree)
      nil
      (tree->seq tree)))
  ; get value
  (valAt [this key default]
    (let [result (ram-find tree key cmp)]
      (if (nil? result)
        default
        (let [[k v] result]
          v))))
  (valAt [this key]
    (.valAt this key nil))
  ; with default value
  clojure.lang.Indexed
  (nth [this i]
    (get-by-index tree i (fn [t] (throw (ex-info "Index out of bounds."
                                          {:type :RandomAccessMap/nth/IndexOutOfBounds
                                           :index i
                                           :size (size tree)})))))
  (nth [this i default]
    (get-by-index tree i (fn [t] default)))
  clojure.lang.IFn
  (invoke [this n] (get this n))
  java.lang.Object
  (hashCode [this] (+ (* 31 (.hashCode tree)) 17))
  (equals [this other]
    (and (instance? other RandomAccessMap)
         (.equals tree (.tree other))))
  (toString [this] (pr-str this)))

(defmethod print-method RandomAccessMap [o ^java.io.Writer w]
  (.write w (str "#RandomAccessMap " (pr-str (.cmp o)) " " (pr-str (.tree o)))))

(defn ->RandomAccessMap
  ([cmp]
     (RandomAccessMap. cmp (empty-ram)))
  ([]
     (RandomAccessMap. compare (empty-ram))))
