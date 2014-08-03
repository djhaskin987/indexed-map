(ns random-access-map.core-test
  (:require [clojure.test :refer :all]
            [random-access-map.core :refer :all]))

(defn- violates-red-invariant?
  "Determines whether there are any red-red parent-child pairs in the tree."
  [tree]
  (if (empty? tree)
    false
    (if (and (= (color tree) :red)
             (or (and (not (empty? (ltree tree)))
                      (= :red (color (ltree tree))))
                 (and (not (empty? (rtree tree)))
                      (= :red (color (rtree tree))))))
      true
      (or (violates-red-invariant? (ltree tree))
          (violates-red-invariant? (rtree tree))))))

(defn- height
  "Computes the height of a tree."
  [tree]
  (if (empty? tree)
    0
    (max (inc (height (ltree)))
         (inc (height (rtree))))))

(defn- black-height
  "Computes the black height of a tree."
  [tree]
  (if (empty? tree)
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
  (if (empty? tree)
    false
    (if (= (black-height (ltree tree)) (black-height (rtree tree)))
      false
      true)))

(defn- balanced?
  "Determines if a red-black tree is balanced"
  [tree]
  (not (or (violates-red-invariant? tree)
           (violates-black-invariant? tree))))

(deftest random-access-map-insert-balanced
  "Testing for proper balancing on insert"
  (testing "Inserting a list in ascending order from 1 to 10..."
    (is (balanced? (reduce insert-val nil (range 10)))))
  (testing "Inserting a list in descending order from 10 to 1..."
    (is (balanced? (reduce insert-val nil (range 10 0 -1)))))
  (testing "Inserting a list in a wierd, but orderly, order..."
    (is (balanced? (reduce insert-val nil [5 4 6 3 7 2 8 1 9 0 10]))))
  (testing "Checking an empty set for balance..."
    (is (balanced? nil)))
  (testing "Checking for balance of a one-item set..."
    (is (balanced? (insert-val nil 1))))
  (testing "Some good stuff: Inserting a list in ascending order from 1 to 100..."
    (is (balanced? (reduce insert-val nil (range 100)))))
  (testing "Some good stuff: Inserting a list in descending order from 100 to 1..."
    (is (balanced? (reduce insert-val nil (range 100 1 -1))))))
