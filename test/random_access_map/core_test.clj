(ns random-access-map.core-test
  (:require [clojure.test :refer :all]
            [clojure.core.match :refer [match]]
            [random-access-map.core :refer :all]))

(defn- violates-red-invariant?
  "Determines whether there are any red-red parent-child pairs in the tree."
  [tree]
  (if (ras-empty? tree)
    false
    (if (and (= (color tree) :red)
             (or (and (not (ras-empty? (ltree tree)))
                      (= :red (color (ltree tree))))
                 (and (not (ras-empty? (rtree tree)))
                      (= :red (color (rtree tree))))))
      true
      (or (violates-red-invariant? (ltree tree))
          (violates-red-invariant? (rtree tree))))))

(defn- height
  "Computes the height of a tree."
  [tree]
  (if (ras-empty? tree)
    0
    (max (inc (height (ltree)))
         (inc (height (rtree))))))

(defn- black-height
  "Computes the black height of a tree."
  [tree]
  (if (ras-empty? tree)
    1
    (let [node-count
          (if (= :black (color tree))
            1
            0)]
      (+ node-count
         (max (black-height (ltree tree))
              (black-height (rtree tree)))))))

(defn- violates-black-invariant?
  "Determines whether tree has unequal black heights for its branches."
  [tree]
  (if (ras-empty? tree)
    false
    (if (= (black-height (ltree tree)) (black-height (rtree tree)))
      false
      true)))

(defn- balanced?
  "Determines if a red-black tree is balanced"
  [tree]
  (not (or (violates-red-invariant? tree)
           (violates-black-invariant? tree))))

(defn i [a b]
  (insert-val a b b (fn [t] t)))

(defn r [a b]
  (remove-val a b (fn [t] t)))

(def inserted-in-order
  (reduce i (empty-ras) (range 10)))

(def inserted-in-reverse-order
  (reduce i (empty-ras) (range 9 -1 -1)))

(def inserted-in-wierd-order
  (reduce i (empty-ras) [5 4 6 3 7 2 8 1 9 0 10]))

(def large-set
  (reduce i (empty-ras) (range 100)))

(def large-reverse-set
  (reduce i (empty-ras) (range 99 -1 -1)))

(def first-removed-tree
  (reduce r large-set (range 2 50 2)))

(def second-removed-tree
  (reduce r inserted-in-order [1 3 5 6]))

(def even-removed
  (reduce
   r
   (reduce i (empty-ras) (range 10))
   [0 2 4 6 8]))

(deftest random-access-map-remove-balanced
  "Testing the remove part of this"
    (testing "Removed even numbers up to 50 from set of 100..."
      (is (balanced? first-removed-tree)))
    (testing "Removed some numbers but not others from set of 20..."
      (is (balanced? second-removed-tree)))
    (testing "Removing from a list of 10..."
      (is (balanced? even-removed)))
    (testing "Removing from the empty list..."
      (is (= (r (empty-ras) 1) (empty-ras))))
    (testing "Removing element that doesn't exist..."
      (is (= first-removed-tree (r first-removed-tree 101)))))

(deftest random-access-map-insert-balanced
  "Testing for proper balancing on insert"
  (testing "Inserting a list in ascending order from 1 to 10..."
    (is (balanced? inserted-in-order)))
  (testing "Inserting a list in descending order from 10 to 1..."
    (is (balanced? inserted-in-reverse-order)))
  (testing "Inserting a list in a wierd, but orderly, order..."
    (is (balanced? inserted-in-wierd-order)))
  (testing "Checking an empty set for balance..."
    (is (balanced? (empty-ras))))
  (testing "Checking for balance of a one-item set..."
    (is (balanced? (i (empty-ras) 1))))
  (testing "Some good stuff: Inserting a list in ascending order from 1 to 100..."
    (is (balanced? large-set)))
  (testing "Some good stuff: Inserting a list in descending order from 100 to 1..."
    (is (balanced? large-reverse-set))))



(defn- actual-count [tree]
  (match [tree]
         [:black-leaf] 0
         [:double-black-leaf] 0
         [[c l k v s r]] (+ (actual-count l) (actual-count r) 1)
         :else
         (ex-info "Actual count called on a non-tree."
                  {:type :ram-test/actual-count/invalid-input
                   :tree tree})))
(deftest count-enforced
  "Check to see that all sizes are everywhere correct in the tree."
  (testing "Checking empty set..."
    (is (= (actual-count (empty-ras)) (size (empty-ras)) 0)))
  (testing "Checking inserted-in-order set..."
    (is (= (actual-count inserted-in-order) (size inserted-in-order))))
  (testing "Checking inserted-in-reverse-order set..."
    (is (= (actual-count inserted-in-reverse-order) (size inserted-in-reverse-order))))
  (testing "Checking inserted-in-wierd-order set..."
    (is (= (actual-count inserted-in-wierd-order) (size inserted-in-wierd-order))))
  (testing "Checking large-set set..."
    (is (= (actual-count large-set) (size large-set))))
  (testing "Checking large-reverse-set set..."
    (is (= (actual-count large-reverse-set) (size large-reverse-set))))
  (testing "Checking first-removed-tree set..."
    (is (= (actual-count first-removed-tree) (size first-removed-tree))))
  (testing "Checking second-removed-tree set..."
    (is (= (actual-count second-removed-tree) (size second-removed-tree))))
  (testing "Checking even-removed set..."
    (is (= (actual-count even-removed) (size even-removed)))))

(deftest test-find-val
  "Test find-val"
  (testing "Find a value in the set..."
    (is (= (find-val inserted-in-order 1 (fn [t] t)) 1)))
  (testing "Don't find a value in the set..."
    (is (thrown? clojure.lang.ExceptionInfo
                 (find-val inserted-in-order 11
                           (fn [t] (throw (ex-info
                                           "Not found."
                                           {:type :ram-test/find-val})))))))
  (testing "Don't find a value -- nil version..."
    (is (= nil (find-val inserted-in-order 11 (fn [t] nil)))))
  (testing "Don't find anything in the empty set..."
    (is (thrown? clojure.lang.ExceptionInfo
                 (find-val (empty-ras) nil
                           (fn [t] (throw
                                    (ex-info
                                     "Not found."
                                     {:type :ram-test/find-val}))))))))
(deftest get-by-rank-test
  "Find out if get-by-rank works."
  (testing "Checking rank on odd-numbered set..."
    (is (= (get (get-by-index even-removed 0) 1) 1))
    (is (= (get (get-by-index even-removed 1) 3) 3))
    (is (= (get (get-by-index even-removed 2) 5) 5))
    (is (= (get (get-by-index even-removed 3) 7) 7))
    (is (= (get (get-by-index even-removed 4) 9) 9)))
  (testing "Checking exception"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Index went out of bounds."
                          (get-by-index even-removed 5)))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Index went out of bounds."
                          (get-by-index even-removed -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inserted-in-reverse-order ;;
;; inserted-in-wierd-order   ;;
;; large-set                 ;;
;; large-reverse-set         ;;
;; first-removed-tree        ;;
;; second-removed-tree       ;;
;; even-removed              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
