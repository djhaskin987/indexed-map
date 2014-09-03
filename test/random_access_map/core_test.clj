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

(defn set->map
  [s]
  (reduce conj {}
          (map (fn [k v] (kvp k v))
               (range (count s))
               (sort (seq s)))))

(defn standard-print [m s]
  (str "\nThe tested map is:\n" m "\nThe tested coll is:\n" s))

(defn standard-tests [name m s]
  (testing (str "Testing the basics of " name "." (standard-print m s))
    (is (balanced? m))
    (is (valid-colors? m))
    (is (= (count m) (actual-count m))))
  (doseq [thing (seq s)]
    (testing (str "Testing find and get of " thing " in " name "." (standard-print m s))
      (is (= (find m (k thing)) (kvp thing)))
      (is (= (get m (k thing)) thing))))
  (let [e (vec (sort (seq s)))]
    ((fn [v x]
            (if (>= x (count v))
              nil
              (do
                (testing (str "Testing nth of " x " in " name "."
                              (standard-print m s)
                              "\nThe testing vector is:\n"
                              v)
                  (is (= (nth m x) (kvp (nth v x)))))
                  (recur v (inc x))))) e 0)))

(defn test-ranges [name
                   ins-range
                   rm-range
                   rmn-range]
  (let [first-map (reduce i (->RandomAccessMap) ins-range)
        first-img (reduce conj #{} ins-range)]
    (standard-tests (str "insert on " name) first-map first-img)
    (let [r-map (reduce r first-map rm-range)
          r-img (reduce disj first-img rm-range)]
      (standard-tests (str "remove on " name) r-map r-img))
    (let [rn-map (reduce rn first-map rmn-range)
          rn-img (set (reduce vec-remove-nth (vec (sort first-img)) rmn-range))]
      (standard-tests (str "remove-nth on " name) rn-map rn-img))))
(deftest empty-tests
  "Testing the empty maps."
  (test-ranges "empty" '() '() '()))
(deftest small-tests
  "Testing the small maps."
  (test-ranges "increasing order, 0 through 9" (range 10) '(1 3 5 6) '(5 4 3 2 1))
  (test-ranges "decreasing order, 9 through 0" (range 9 -1 -1) '(0 3 2 4) (repeat 3 0)))
