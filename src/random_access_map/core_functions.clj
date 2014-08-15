(in-ns 'random-access-map.core)

(defn empty-ram
  "Creates an empty ram."
  []
  :black-leaf)

(defn ram-empty?
  "Determines if a tree is empty."
  [tree]
  (match [tree]
         [:black-leaf] true
         [:double-black-leaf] true
         :else false))

(defn color
  "Gets the color of a tree node."
  [tree]
  (match [tree]
         [[c _ _ _ _ _]] c
         [:black-leaf] :black
         [:double-black-leaf] :double-black))
(defn ltree
  [tree]
  (let [[_ l _ _ _ _] tree]
    l))
(defn rtree
  [tree]
  (let [[_ _ _ _ _ r] tree]
    r))



(defn size
  "Gets the size of a tree node."
  [tree]
  (match [tree]
         [[_ _ _ _ s _]] s
         [:black-leaf] 0
         [:double-black-leaf] 0
         :else
         (throw (ex-info "Size called on a non-tree."
                  {:type :ram/size/invalid-input
                   :bad-data tree}))))

(defn- decblack
  [c]
  (match c
         :black :red
         :double-black :black
         :red :negative-black
         :else c))

(defn- lighten
  [tree]
  (match tree
         [c a k v s b] [(decblack c) a k v s b]
         :double-black-leaf :black-leaf
         :else tree))

(defn- incblack
  [c]
  (match c
         :black :double-black
         :red :black
         :negative-black :red
         :else c))

(defn- darken
  [tree]
  (match tree
         [c a k v s b] [(incblack c) a k v s b]
         :black-leaf :double-black-leaf
         :else tree))
(defn update-size
  [c l k v r]
  [c l k v (+ 1 (size l) (size r)) r])

(defn- balance
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [tree]
  (match [tree]
         [(:or ;; Left child red with left red grandchild
               [(:or :black :double-black) [:red [:red a kx vx _ b] ky vy _ c] kz vz _ d]
               ;; Left child red with right red grandchild
               [(:or :black :double-black) [:red a kx vx _ [:red b ky vy _ c]] kz vz _ d]
               ;; Right child red with left red grandchild
               [(:or :black :double-black) a kx vx _ [:red [:red b ky vy _ c] kz vz _ d]]
               ;; Right child red with right red grandchild
               [(:or :black :double-black) a kx vx _ [:red b ky vy _  [:red c kz vz _ d]]])]
         ; =>
         (update-size (decblack (color tree))
                      (update-size :black a kx vx b)
                      ky vy
                      (update-size :black c kz vz d))
         [[:double-black [:negative-black
                          [:black a kw vw sw b]
                          kx vx _
                          [:black c ky vy _ d]]
           kz vz _
           e]]
           (update-size :black (update-size :black
                                            (balance [:red a kw vw sw b])
                                            kx vx c)
            ky vy
            (update-size :black d kz vz e))
         ; now the symmetric case ...
         [[:double-black e kz vz _
           [:negative-black
            [:black d ky vy _ c]
            kx vx _
            [:black b kw vw sw a]]]]
               ; =>
            (update-size :black
                         (update-size :black e kz vz d)
                         ky vy
                         (update-size :black c kx vx (balance [:red b kw vw sw a])))
            :else
            tree))

(defn insert-val
  "Inserts x in tree.
  Returns a node with x and no children if tree is empty.
  Returned tree is balanced. See also `balance`"
  [tree kx vx df cmp]
  (let [ins (fn ins [tree]
              (match tree
                     :black-leaf [:red :black-leaf kx vx 1 :black-leaf]
                     [c a ky vy sy b] (case (cmp kx ky)
                                        -1 (balance (update-size c (ins a) ky vy b))
                                         0 (df tree)
                                         1 (balance (update-size c a ky vy (ins b))))))
        [_ a ky vy sy b] (ins tree)] [:black a ky vy sy b]))

(defn- bubble
  "Suds and bath water!"
  [c l k v s r]
  (if
      (or (= (color l) :double-black)
          (= (color r) :double-black))
    (balance [(incblack c) (lighten l) k v s (lighten r)])
    [c l k v s r]))

(declare remove-raw)

(defn remove-max
  "Remove the maximum element of a tree."
  [tree]
  (let [[c a kx vx sx b] tree]
    (if (ram-empty? b)
      [kx vx (remove-raw tree)]
      (let [[kr vr b'] (remove-max b)]
        [kr vr (bubble c a kx vx (+ 1 (size a) (size b')) b')]))))

(defn- remove-raw
  "Compute a new tree with value removed, except unbalanced at first."
  [tree]
  (match tree
         [:red :black-leaf _ _ _ :black-leaf] :black-leaf
         [:black :black-leaf _ _ _ :black-leaf] :double-black-leaf
         (:or [:black :black-leaf _ _ _ [:red a ky vy sy b]]
               [:black [:red a ky vy sy b] _ _ _ :black-leaf])
         ; =>
         [:black a ky vy sy b]
         :else
         (let [[c l kx vx sx r] tree
               [kr vr l'] (remove-max l)]
           (update-size c l' kr vr r))))

; tree key -> tree
(defn remove-val
  "Compute a new tree with value removed."
  [tree key df cmp]
  (if (ram-empty? tree)
    (df tree)
    (let [[c l k v s r] tree]
      (case (cmp key k)
        -1 (let [new-tree (remove-val l key df cmp)]
              (bubble c new-tree k v (+ 1 (size r) (size new-tree)) r))
         0 (remove-raw tree)
         1 (let [new-tree (remove-val r key df cmp)]
              (bubble c l k v (+ 1 (size l) (size new-tree)) new-tree))))))

(defn ram-find
  "Finds value x in tree"
  [tree x cmp]
  (match [tree]
         [:black-leaf] nil
         [[_ a kx vx sx b]] (case (cmp x kx)
                              -1 (recur a x cmp)
                               0 [kx vx]
                               1 (recur b x cmp))))

(defn get-by-index
  "Retrieves a pair based on rank."
  [tree index df]
  (if (ram-empty? tree)
    (df tree)
    (let [[_ l k v _ r] tree
          left-size (size l)]
      (case (compare index left-size)
            -1 (get-by-index l index df)
             0 [k v]
             1 (get-by-index r (- index left-size 1) df)))))
