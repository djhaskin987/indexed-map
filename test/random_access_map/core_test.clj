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
    (max (inc (height (ltree)))
         (inc (height (rtree))))))

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
  [tree]
  (let [t (.tree tree)]
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
         (naive-balanced? r))))))

(defn balanced?
  "Determines if a red-black tree is balanced"
  [tree]
  (let [t (.tree tree)]
    (and (not (or (violates-red-invariant? t)
                  (violates-black-invariant? t)))
         (naive-balanced? tree))))

(defn i [a b]
  (assoc a (keyword (str b)) b))

(defn r [a b]
  (dissoc a (keyword (str b))))

(defn rn [a b]
  (disjoin-nth a b))

(def inserted-in-order
  (reduce i (->RandomAccessMap) (range 10)))

(def inserted-in-reverse-order
  (reduce i (->RandomAccessMap) (range 9 -1 -1)))

(def inserted-in-wierd-order
  (reduce i (->RandomAccessMap) [5 4 6 3 7 2 8 1 9 0]))

(def large-set
  (reduce i (->RandomAccessMap) (range 100)))

(def large-reverse-set
  (reduce i large-set (range 99 -1 -1)))

(def first-removed-tree
  (reduce r large-set (range 2 50 2)))

(def second-removed-tree
  (reduce r inserted-in-order [1 3 5 6]))

(def even-removed
  (reduce
   r
   (reduce i (->RandomAccessMap) (range 10))
   [0 2 4 6 8]))

(def first-index-removed-tree
  (reduce rn large-set (range 0 40 3)))

(def second-index-removed-tree
  (reduce rn inserted-in-order [0 2 4 5]))

(def start-index-removed-tree
  (reduce rn inserted-in-order (repeat 0 5)))

(def end-index-removed-tree
  (reduce rn inserted-in-order (range 9 4 -1)))

(def all-maps
  [inserted-in-order
   inserted-in-reverse-order
   inserted-in-wierd-order
   large-reverse-set
   first-removed-tree
   second-removed-tree
   even-removed
   first-index-removed-tree
   second-index-removed-tree
   start-index-removed-tree
   end-index-removed-tree])

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

(defn actual-count [tree]
  (let [t (.tree tree)]
    (match [t]
           [:black-leaf] 0
           [:double-black-leaf] 0
           [[c l k v s r]] (+ (actual-count l) (actual-count r) 1)
           :else
           (ex-info "Actual count called on a non-tree."
                    {:type :ram-test/actual-count/invalid-input
                     :tree tree})))

(deftest random-access-map-insert-balanced
  "Testing for proper balancing on insert"
  (testing "ALL THE THINGS..."
    (doseq (fn [m]
             (is (balanced? m))
             (is (valid-colors? m)))
             all-maps)))
#_((testing "Inserting a list in ascending order from 1 to 10..."
    (is (balanced? inserted-in-order)))
  (testing "Inserting a list in descending order from 10 to 1..."
    (is (balanced? inserted-in-reverse-order)))
  (testing "Inserting a list in a wierd, but orderly, order..."
    (is (balanced? inserted-in-wierd-order)))
  (testing "Checking an empty set for balance..."
    (is (balanced? (->RandomAccessMap))))
  (testing "Checking for balance of a one-item set..."
    (is (balanced? (i (->RandomAccessMap) 1))))
  (testing "Some good stuff: Inserting a list in ascending order from 1 to 100..."
    (is (balanced? large-set)))
  (testing "Some good stuff: Inserting a list in descending order from 100 to 1..."
    (is (balanced? large-reverse-set))))

#_(deftest random-access-map-remove-balanced
  "Testing the remove part of this"
    (testing "Removed even numbers up to 50 from set of 100..."
      (is (balanced? first-removed-tree)))
    (testing "Removed some numbers but not others from set of 20..."
      (is (balanced? second-removed-tree)))
    (testing "Removing from a list of 10..."
      (is (balanced? even-removed)))
    (testing "Removing from the empty list..."
      (is (= (r (->RandomAccessMap) 1) (->RandomAccessMap))))
    (testing "Removing element that doesn't exist..."
      (is (= first-removed-tree (r first-removed-tree 101)))))

#_(deftest random-access-map-remove-colors
  "Checks colors are correct after removal."
    (testing "Checking colors on first tree..."
      (is (valid-colors? (.tree first-removed-tree))))
    (testing "Checking colors on second removed tree..."
      (is (valid-colors? (.tree second-removed-tree))))
    (testing "Checking colors on even removed tree..."
      (is (valid-colors? (.tree even-removed)))))

#_(deftest random-access-map-index-remove-balanced
  "Testing the remove part of this"
    (testing "Removed every third index from 0 to 40 from a set of 100..."
      (is (balanced? first-index-removed-tree)))
    (testing "Removed by some indexes but not by others from a set of 10..."
      (is (balanced? second-index-removed-tree)))
    (testing "Removing by index from a list of 10 from the end..."
      (is (balanced? end-index-removed-tree)))
    (testing "Removing by index from a list of 10 from the beginning..."
      (is (balanced? end-index-removed-tree)))
    (testing "Removing from the empty list..."
      (is (= (rn (->RandomAccessMap) 0) (->RandomAccessMap))))
    (testing "Removing element that doesn't exist..."
      (is (= first-removed-tree (rn first-removed-tree 101)))))

#_(deftest random-access-map-index-remove-colors
  "Checks colors are correct after removal."
    (testing "Checking colors on first index-removed tree..."
      (is (valid-colors? (.tree first-index-removed-tree))))
    (testing "Checking colors on second index-removed tree..."
      (is (valid-colors? (.tree second-index-removed-tree))))
    (testing "Checking colors on start indexed removed tree..."
      (is (valid-colors? (.tree start-index-removed-tree))))
    (testing "Checking colors on end indexed removed tree..."
      (is (valid-colors? (.tree end-index-removed-tree)))))



#_(deftest count-enforced
  "Check to see that all sizes are everywhere correct in the tree."
  (testing "Checking empty set..."
    (is (= (actual-count (empty-ram)) (size (empty-ram)) 0)))
  (testing "Checking inserted-in-order set..."
    (is (= (actual-count (.tree inserted-in-order)) (count inserted-in-order))))
  (testing "Checking inserted-in-reverse-order set..."
    (is (= (actual-count (.tree inserted-in-reverse-order)) (count inserted-in-reverse-order))))
  (testing "Checking inserted-in-wierd-order set..."
    (is (= (actual-count (.tree inserted-in-wierd-order)) (count inserted-in-wierd-order))))
  (testing "Checking large-set set..."
    (is (= (actual-count (.tree large-set)) (count large-set))))
  (testing "Checking large-reverse-set set..."
    (is (= (actual-count (.tree large-reverse-set)) (count large-reverse-set))))
  (testing "Checking first-removed-tree set..."
    (is (= (actual-count (.tree first-removed-tree)) (count first-removed-tree))))
  (testing "Checking second-removed-tree set..."
    (is (= (actual-count (.tree second-removed-tree)) (count second-removed-tree))))
  (testing "Checking even-removed set..."
    (is (= (actual-count (.tree even-removed)) (count even-removed))))
  (testing "Checking second-removed-tree set..."
    (is (= (actual-count (.tree second-removed-tree)) (count second-removed-tree))))
  (testing "Checking first-indexed-removed-tree set..."
    (is (= (actual-count (.tree first-indexed-removed-tree)) (count second-removed-tree))))
  (testing "Checking second-indexed-removed-tree set..."
    (is (= (actual-count (.tree second-indexed-removed-tree)) (count second-removed-tree))))
  (testing "Checking start-indexed-removed-tree set..."
    (is (= (actual-count (.tree start-indexed-removed-tree)) (count second-removed-tree))))
  (testing "Checking end-indexed-removed-tree set..."
    (is (= (actual-count (.tree end-indexed-removed-tree)) (count second-removed-tree)))))

;; UNCOMMENT THIS
#_(deftest test-ram-find
  "Test ram-find"
  (testing "Testing find for inserted-in-order..."
    (is (= (find inserted-in-order :1) (clojure.lang.MapEntry. :1 1)))
    (is (= (find inserted-in-order :4) (clojure.lang.MapEntry. :4 4)))
    (is (= (find inserted-in-order :7) (clojure.lang.MapEntry. :7 7))))
  (testing "Testing find for inserted-in-reverse-order..."
    (is (= (find inserted-in-reverse-order :2) (clojure.lang.MapEntry. :2 2)))
    (is (= (find inserted-in-reverse-order :3) (clojure.lang.MapEntry. :3 3)))
    (is (= (find inserted-in-reverse-order :8) (clojure.lang.MapEntry. :8 8))))
  (testing "Testing find for inserted-in-wierd-order..."
    (is (= (find inserted-in-wierd-order :5) (clojure.lang.MapEntry. :5 1)))
    (is (= (find inserted-in-wierd-order :6) (clojure.lang.MapEntry. :6 4)))
    (is (= (find inserted-in-wierd-order :9) (clojure.lang.MapEntry. :9 7))))

;; inserted-in-reverse-order
;; inserted-in-wierd-order
;; large-reverse-set
;; first-removed-tree
;; second-removed-tree
;; even-removed
;; first-index-removed-tree
;; second-index-removed-tree
;; start-index-removed-tree
;; end-index-removed-tree



  (testing "Find a value in the set..."
    (is (= (get inserted-in-order :1) 1)))
  (testing "Don't find a value in the set..."
    (is (nil? (get inserted-in-order :11))))
  (testing "Don't find anything in the empty set..."
    (is (nil? (get (->RandomAccessMap) :1)))))

;; UNCOMMENT THIS
#_(deftest get-by-rank-test
  "Find out if get-by-rank works."
  (testing "Checking rank on odd-numbered set..."
    (is (= (nth (nth even-removed 0) 1) 1))
    (is (= (nth (nth even-removed 1) 1) 3))
    (is (= (nth (nth even-removed 2) 1) 5))
    (is (= (nth (nth even-removed 3) 1) 7))
    (is (= (nth (nth even-removed 4) 1) 9)))
  (testing "Checking exception"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Index out of bounds."
                          (nth even-removed 5)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Index out of bounds."
                          (nth even-removed -1))))
    (testing "Checking default value"
      (is (= (nth even-removed 87 :armadillo) :armadillo))
      (is (= (nth even-removed 303 nil) nil))
      (is (= (nth even-removed -1 "whodj") "whodj"))))
