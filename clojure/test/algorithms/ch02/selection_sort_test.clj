(ns algorithms.ch02.selection-sort-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.sorting :refer :all]
            [algorithms.ch02.selection-sort :refer :all]))

(deftest test-selection-sort
  (testing "selection sort"
    (is (is-sorted-asc (selection-sort [])))
    (is (is-sorted-asc (selection-sort [1])))
    (is (is-sorted-asc (selection-sort [1 2 3 4 5])))
    (is (is-sorted-asc (selection-sort [5 1 3 2 4])))
    (is (equal (selection-sort [9 1 8 2 7 3 6 4 5 0]) [0 1 2 3 4 5 6 7 8 9]))
    (is (is-sorted-asc (selection-sort (random-vec 1000 0 1000))))))
