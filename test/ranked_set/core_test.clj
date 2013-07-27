(ns ranked-set.core-test
  (:require [clojure.test :refer :all]
            [ranked-set.core :refer :all]))
(import 'ranked_set.core.ranked_set)
(deftest nil-add
  (testing "Adding to a nil ranked set.")
             (is (= (add nil 1 2) nil)))
(deftest nil-retrieve
  (testing "Retrieving from a nil ranked set.")
         (is (= (retrieve nil 1) nil)))
(deftest nil-cut
  (testing "Cutting from a nil ranked set.")
         (is (= (cut nil 1) nil)))
(deftest nil-size
  (testing "Getting size from a nil set.")
         (is (= (size nil) 0)))

;; Add a test for erroring on incorrect rank given to the functions (<0,>size)
;;
