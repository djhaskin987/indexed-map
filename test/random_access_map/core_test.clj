(ns random-access-map.core-test
  (:require [clojure.test :refer :all]
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

(deftest random-access-map-remove-balanced
  "Testing the remove part of this"
  (let [first-removed-tree
        (reduce
         remove-val
         (reduce insert-val (empty-ras) (range 1 100))
         (range 2 50 2))
        second-removed-tree
        (reduce
         remove-val
         (reduce insert-val (empty-ras) (range 1 20))
         [1 3 5 6 8 10])]
    (testing "Removed even numbers up to 50 from set of 100..."
      (is (balanced? first-removed-tree)))
    (testing "Removed some numbers but not others from set of 20..."
      (is (balanced? second-removed-tree)))
  (testing "Removing from a list of 10..."
    (is (balanced? (reduce
                    remove-val
                    (reduce insert-val (empty-ras) (range 10))
                    [2 4 6 8 10]))))
  (testing "Removing from the empty list..."
    (is (= (remove-val (empty-ras) 1) (empty-ras))))
  (testing "Removing element that doesn't exist..."
    (is (= first-removed-tree (remove-val first-removed-tree 0))))))

(deftest random-access-map-insert-balanced
  "Testing for proper balancing on insert"
  (testing "Inserting a list in ascending order from 1 to 10..."
    (is (balanced? (reduce insert-val (empty-ras) (range 10)))))
  (testing "Inserting a list in descending order from 10 to 1..."
    (is (balanced? (reduce insert-val (empty-ras) (range 10 0 -1)))))
  (testing "Inserting a list in a wierd, but orderly, order..."
    (is (balanced? (reduce insert-val (empty-ras) [5 4 6 3 7 2 8 1 9 0 10]))))
  (testing "Checking an empty set for balance..."
    (is (balanced? (empty-ras))))
  (testing "Checking for balance of a one-item set..."
    (is (balanced? (insert-val (empty-ras) 1))))
  (testing "Some good stuff: Inserting a list in ascending order from 1 to 100..."
    (is (balanced? (reduce insert-val (empty-ras) (range 100)))))
  (testing "Some good stuff: Inserting a list in descending order from 100 to 1..."
    (is (balanced? (reduce insert-val (empty-ras) (range 100 1 -1))))))
