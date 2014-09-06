(ns random-access-map.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.match :refer [match]]
            [random-access-map.core :refer :all]))

(defn violates-red-invariant?
  "Determines whether there are any red-red parent-child pairs in the tree."
  [tree]
  (if (ram-empty? tree)
    false
    (if (and (= (color tree) :red)
             (or (and (not (ram-empty? (ltree tree)))
                      (= :red (color (ltree tree))))
                 (and (not (ram-empty? (rtree tree)))
                      (= :red (color (rtree tree))))))
      true
      (or (violates-red-invariant? (ltree tree))
          (violates-red-invariant? (rtree tree))))))

(defn find-red-violation
  "Determines whether there are any red-red parent-child pairs in the tree."
  [tree]
  (let [list-builder (fn list-builder [t l]
                       (if (ram-empty? t)
                         l
                         (if (and (= (color t) :red)
                                  (or (and (not (ram-empty? (ltree t)))
                                           (= :red (color (ltree t))))
                                      (and (not (ram-empty? (rtree t)))
                                           (= :red (color (rtree t))))))
                           (list-builder (rtree t) (list-builder (ltree t) (cons t l))))))]
    (list-builder tree '())))

(defn height
  "Computes the height of a tree."
  [tree]
  (if (ram-empty? tree)
    0
    (max (inc (height (ltree tree)))
         (inc (height (rtree tree))))))

(defn black-height
  "Computes the black height of a tree."
  [tree]
  (if (ram-empty? tree)
    1
    (let [node-count
          (if (= :black (color tree))
            1
            0)]
      (+ node-count
         (max (black-height (ltree tree))
              (black-height (rtree tree)))))))

(defn violates-black-invariant?
  "Determines whether tree has unequal black heights for its branches."
  [tree]
  (if (ram-empty? tree)
    false
    (if (= (black-height (ltree tree)) (black-height (rtree tree)))
      false
      true)))

(defn naive-balanced?
  [t]
    (if (ram-empty? t)
      true
      ;; check heights recursively
      (let [l (ltree t)
            r (rtree t)
            lh (height l)
            rh (height r)]
        (and
         (if (< lh rh)
           (<= rh (+ (* 2 lh) 1))
           (if (< rh lh)
             (<= lh (+ (* 2 rh) 1))
             true))
         (naive-balanced? l)
         (naive-balanced? r)))))

(defn balanced?
  "Determines if a red-black tree is balanced"
  [tree]
  (let [t (.tree tree)]
    (and (not (or (violates-red-invariant? t)
                  (violates-black-invariant? t)))
         (naive-balanced? t))))

(defn k [a]
  (keyword (str a)))

(defn i [a b]
  (assoc a (k b) b))

(defn r [a b]
  (dissoc a (keyword (str b))))

(defn rn [a b]
  (disjoin-nth a b))

(defn actual-count [tree]
  (let [t (.tree tree)
        rec (fn rec [t]
              (match [t]
                       [:black-leaf] 0
                       [:double-black-leaf] 0
                       [[c l k v s r]] (+ (rec l) (rec r) 1)
                       :else
                       (ex-info "Actual count called on a non-tree."
                                {:type :ram-test/actual-count/invalid-input
                                 :tree tree})))]
    (rec t)))
(defn kvp
  ([v]
     (clojure.lang.MapEntry. (k v) v))
  ([a b]
     (clojure.lang.MapEntry. (k a) b)))

(defn valid-colors?
  "Checks to see if all colors are either red or black."
  [tree]
  (let [t (.tree tree)
        f (fn f [t]
            (if (keyword? t)
              (= t :black-leaf)
              (let [[c l k v s r] t]
                (and (or (= :black c) (= :red c))
                     (f l)
                     (f r)))))]
    (f t)))

(defn vec-remove-nth
  [v i]
  (vec (concat (subvec v 0 i) (subvec v (inc i) (count v)))))

(defn vec-remove-yield-image
  [v c x]
  [(conj c (nth v x))
   (vec-remove-nth v x)])

(defn vec-remove-build-image
  [a b]
  (let [[img vect] a]
    (vec-remove-yield-image vect img b)))

(defn set->map
  [s]
  (reduce conj {}
          (map (fn [k v] (kvp k v))
               (range (count s))
               (sort (seq s)))))

(defn standard-print [m s]
  (str "\nThe tested map is:\n" m "\nThe tested coll is:\n" s))

(defn index-bound-tests [name m]
  (testing (str "Testing nth on out of bound indexes of " name ".")
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Index out of bounds." (nth m -1)))
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Index out of bounds." (nth m (count m))))
      (is (= (nth m -1 :default) :default))
      (is (= (nth m (count m) :default) :default))))

(defn absence-tests [name m s]
  (doseq [thing (seq s)]
    (testing (str "Testing find and get of absent things for " thing " in " name "." (standard-print m s))
      (is (= (find m (k thing)) nil))
      (is (= (get m (k thing)) nil)))))

(defn standard-tests [name m]
  (testing (str "Testing the basics of " name ".")
    (is (balanced? m))
    (is (valid-colors? m))
    (is (= (count m) (actual-count m)))))

(defn presence-tests [name m s]
  (doseq [thing (seq s)]
    (testing (str "Testing find and get of " thing " in " name "." (standard-print m s))
      (is (= (find m (k thing)) (kvp thing)))
      (is (= (get m (k thing)) thing))))
    ((fn [v x]
       (if (>= x (count v))
         nil
         (do
           (testing (str "Testing nth of " x " in " name "."
                         (standard-print m s)
                         "\nThe testing vector is:\n"
                         v)
             (is (= (nth m x) (nth v x))))
           (recur v (inc x)))))
     (vec (sort (map kvp (seq s)))) 0))

(defn test-ranges [name
                   ins-range
                   rm-range
                   rmn-range]
  (let [first-map (reduce i (->RandomAccessMap) ins-range)
        first-img (reduce conj #{} ins-range)
        insert-name (str "insert on " name)]
    (standard-tests insert-name first-map)
    (presence-tests insert-name first-map first-img)
    (index-bound-tests insert-name first-map)
    (let [r-map (reduce r first-map rm-range)
          r-img (reduce disj first-img rm-range)
          r-name (str "remove on " name)]
      (standard-tests r-name r-map)
      (presence-tests r-name r-map r-img)
      (absence-tests r-name r-map rm-range)
      (index-bound-tests r-name r-map))
    (let [rn-name (str "remove-nth on " name)
          rn-map (reduce rn first-map rmn-range)
          [removed-img removed-vect] (reduce vec-remove-build-image
                                             ; first, we map kvp to them, so we
                                             ; sort correctly by the keyword
                                             ; version of the number, so that we
                                             ; remove the correct elements from
                                             ; first-img...
                                             [#{} (vec (sort (map kvp first-img)))] rmn-range)
          removed-result (set (sort removed-vect))]
      (standard-tests rn-name rn-map)
      ; Then, since the tests don't expect a key-value pair, rather only a
      ; value, we unwrap the value from its k/v pair so that things
      ; get tested properly here.
      (presence-tests rn-name rn-map (map val removed-result))
      (absence-tests rn-name rn-map (map val removed-img))
      (index-bound-tests rn-name rn-map))))

(deftest small-tests
  "Testing the small maps."
  (test-ranges "increasing order, 0 through 9" (range 10) '(1 3 5 6) '(5 4 3 2 1))
  (test-ranges "decreasing order, 9 through 0" (range 9 -1 -1) '(0 3 2 4) (repeat 3 0)))

(deftest pretty-small-tests
  "Testing the smallest of maps."
  (test-ranges "poquito" (range 3) '(1 2 3) '())
  (test-ranges "little" (range 7) '() '(3 3 2 2 1 1 0)))

(deftest empty-tests
  "Testing the empty maps."
  (test-ranges "empty" '() '(1 2 3 4 5 :heyheyhey) '()))

(deftest midsize-tests
  "A bit bigger, to find more errors."
  (test-ranges "increasing order, 10 to 30." (range 10 30) (range 10 30 4) (range 14 -1 -1))
  (test-ranges "increasing order, 20 to 30 and decreasing order, 70 to 90."
               (concat (range 20 30) (range 90 70 -1)) (range 90 70 -1) (repeat 5 4)))

(deftest large-tests
  "Testing the large maps."
  (test-ranges "pretty big." (range 100) (range 0 50 2) (repeat 20 0))
  (test-ranges "reverse big." (range 99 -1 -1) (range 20 50 3) (range 89 58 -1)))
