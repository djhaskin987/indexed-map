(ns random-access-map.core
  (:require [clojure.core.match :refer [match]])
  (:gen-class))

(def black-leaf [:black])
(def double-black-leaf [:double-black])

(defn empty-ras
  "Creates an empty ras."
  []
  black-leaf)

(defn ras-empty?
  "Determines if a tree is empty."
  [tree]
  (match tree
         [black-leaf] true
         [double-black-leaf] true
         :else false))

(defn color
  "Gets the color of a tree node."
  [tree]
  (nth tree 0))
(defn ltree
  "Gets the left tree of a tree node."
  [tree]
  (nth tree 1))
(defn elem
  "Gets the element of a tree node."
  [tree]
  (nth tree 2))
(defn rtree
  "Gets the right tree of a tree node."
  [tree]
  (nth tree 3))

(defn- decblack
  [color]
  (match [color]
         [:black] :red
         [:double-black] :black
         [:red] :negative-black
         [[color a x b]] [(decblack color) a x b]
         :else color))

(defn- incblack
  [color]
  (match [color]
         [:black] :double-black
         [:red] :black
         [:negative-black] :red
         [[color a x b]] [(incblack color) a x b]
         :else color))

(defn- balance
  "Ensures the given subtree stays balanced by rearranging black nodes
  that have at least one red child and one red grandchild"
  [tree]
  (match [tree]
         [(:or ;; Left child red with left red grandchild
               [(:or :black :double-black) [:red [:red a x b] y c] z d]
               ;; Left child red with right red grandchild
               [(:or :black :double-black) [:red a x [:red b y c]] z d]
               ;; Right child red with left red grandchild
               [(:or :black :double-black) a x [:red [:red b y c] z d]]
               ;; Right child red with right red grandchild
               [(:or :black :double-black) a x [:red b y [:red c z d]]])]
         ; =>
         [(decblack (color tree)) [:black a x b] y [:black c z d]]
         [[:double-black [:negative-black
                          [:black a w b]
                          x
                          [:black c y d]]
           z
           e]]
         [:black
          [:black (balance [:red a w b]) x c]
          y
          [:black d z e]]
         ; now the symmetric case ...
         [[:double-black e z
           [:negative-black
            [:black d y c]
            x
            [:black b w a]]]]
         [:black [:black e z d]
          y
          [:black c x (balance [:red b w a])]]
         :else
         tree))

(defn insert-val
  "Inserts x in tree.
  Returns a node with x and no children if tree is empty.
  Returned tree is balanced. See also `balance`"
  [tree x]
  (let [ins (fn ins [tree]
              (match tree
                     [black-leaf] [:red black-leaf x black-leaf]
                     [color a y b] (let [condition (compare x y)]
                                     (< condition 0) (balance [color (ins a) y b])
                                     (< 0 condition) (balance [color a y (ins b)]))
                                     :else tree))
        [_ a y b] (ins tree)] [:black a y b]))

(declare remove-val)

(defn remove-max
  "Remove the maximum element of a tree."
  [tree]
  (let [[c a x b] tree]
    (if (ras-empty? b)
      [x (remove-raw tree)]
      (let [[el b'] (remove-max b)]
        [el (bubble c a x b')]))))

(defn- bubble
  "suds and bath water!"
  [color left elem right]
  (if
   (or (= (color left) :double-black)
       (= (color right) :double-black))
   (balance [(incblack color) (decblack left) elem (decblack right)])
   [color left elem right]))

(defn- raw-remove
  "Compute a new tree with value removed, except unbalanced at first."
  [tree]
  (match tree
         [[:red black-leaf _ black-leaf]] black-leaf
         [[:black black-leaf _ black-leaf]] double-black-leaf
         [(:or [:black black-leaf x [:red a y b]]
               [:black [:red a y b] x black-leaf])]
         ; =>
         [:black a y b]
         :else
         (let [[c l x r] tree
               [el l'] (remove-max l)]
           [c l' el r])))

(defn remove-val
  "Compute a new tree with value removed."
  [tree val]
  (if (ras-empty? tree)
    tree
    (let [[color left elem right] tree
          condition (compare val elem)]
      (cond (< condition 0) (bubble color (remove-val left val) elem right)
            (< 0 condition) (bubble color left elem (remove-val right val))
            :else
            (remove-raw tree))
  (let [ins (fn ins [tree]
              (match tree
                     black-leaf [:red black-leaf x black-leaf]
                     [color a y b] (let [condition (compare x y)]
                                     (< condition 0) (balance [color (ins a) y b])
                                     (< 0 condition) (balance [color a y (ins b)]))
                                     :else tree)
                     [_ a y b] (ins tree)] [:black a y b])))
(defn find-val
  "Finds value x in tree"
  [tree x]
  (match tree
         [black-leaf] nil
         [_ a y b] (let [condition (compare x y)]
                     (cond
                      (< condition 0) (recur a x)
                      (< 0 condition) (recur b x)
                      :else x))))
