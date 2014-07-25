(ns random-access-map.core-test
  (:require [clojure.test :refer :all]
            [random-access-map.core :refer :all]))

(deftest random-access-map-add-test-nil
         (testing "Adding to a nil set..."
                  (is (= (random-access-map-add nil 1 5)
                         (->RankedNode nil 1 5 1 nil))))
         (testing "Adding to an empty set."
                  (is (= (random-access-map-add (empty-random-access-map) 2 5)
                         (->RankedNode nil 2 5 1 nil)))))

; TODO: Add a test which tests the path of:
; If I attempt to add an element with equivalent magnitude,
; I don't add it to my set.
(deftest random-access-map-add-tree
         (let [first-test-tree
               (->RankedNode
                 (->RankedNode nil 'a 18 1 nil)
                 'b 10 2
                 (->RankedNode nil 'c 2 3 nil))]
           (testing "Creating a tree."
                    (= first-test-tree
                       (random-access-map-add
                         (random-access-map-add
                           (random-access-map-add
                             (empty-random-access-map)
                             'b 10)
                           'a 18)
                         'c 2)))))
