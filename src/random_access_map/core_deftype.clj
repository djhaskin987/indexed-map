(in-ns 'random-access-map.core)
(deftype RandomAccessMap [cmp tree]
  clojure.lang.IPersistentMap
  ; make sure this element doesn't exist.
  (without [this key]
    (if (nil? (ram-find tree key))
      this
      (remove-val tree key (fn [t] (throw (ex-info "Tried to remove a value that didn't exist."
                                                   {:type :RandomAccessMap/without
                                                    :key key}))))))
  ; insert with replacement.
  (assoc [this key val]
    (insert-val tree key val (fn [t]
                               (let [[l k v s r] t]
                                 [l key val s r]))))
  ; says it.
  ;(iterator [this])
  ; says it.
  (containsKey [this key]
    (not (nil? (ram-find tree key))))
  ; vector of [k v], returns nil if it doesn't find it
  (entryAt [this key]
    (ram-find tree key))
  ; get size.
  (count [this]
    (size tree))
  ; get empty collection (nil collection).
  (empty [this]
    (RandomAccessMap. :black-leaf))
  ; takes a [k v] and inserts it.
  (cons [this kvpair]
    (if (nil? kvpair)
      this
      (let [[k v] kvpair]
        (assoc this key val))))
  ; equals?
  (equiv [this other]
    (and (instance? RandomAccessMap other)
         (= tree (.tree other))))
  ; get the seq!
  ; (seq [this])
  ; get value
  (valAt [this key default]
    (let [result (ram-find tree key)]
      (if (nil? result)
        default
        (let [[k v] result]
          v))))
  (valAt [this key]
    (.valAt this key nil))
  ; with default value
  (hashCode [this] (+ (* 31 (.hashCode tree)) 17))
  (equals [this other]
    (and (instance? other RandomAccessMap)
         (.equals tree (.tree other)))))
