(ns ranked-set.core-test
  (:require [clojure.test :refer :all]
            [ranked-set.core :refer :all]))

(deftest ranked-set-add-test-nil
         (testing "Adding to a nil set..."
                  (is (= (ranked-set-add nil 1 5)
                         (->RankedNode nil 1 5 1 nil))))
         (testing "Adding to an empty set."
                  (is (= (ranked-set-add (empty-ranked-set) 2 5)
                         (->RankedNode nil 2 5 1 nil)))))

(deftest ranked-set-add-tree
         (let [first-test-tree
               (->RankedNode
                 (->RankedNode nil 'a 18 1 nil)
                 'b 10 2
                 (->RankedNode nil 'c 2 3 nil))]
           (testing "Creating a tree."
                    (= first-test-tree
                       (ranked-set-add
                         (ranked-set-add
                           (ranked-set-add
                             (empty-ranked-set)
                             'b 10)
                           'a 18)
                         'c 2)))))
