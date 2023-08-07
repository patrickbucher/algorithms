(ns algorithms.ch02.insertion-sort-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.sorting :refer :all]
            [algorithms.ch02.insertion-sort :refer :all]))

(deftest test-insertion-sort
  (testing "insertion sort"
    (is (is-sorted-asc (insertion-sort [])))
    (is (is-sorted-asc (insertion-sort [1])))
    (is (is-sorted-asc (insertion-sort [1 2 3 4 5])))
    (is (is-sorted-asc (insertion-sort [5 1 3 2 4])))
    (is (equal (insertion-sort [9 1 8 2 7 3 6 4 5 0]) [0 1 2 3 4 5 6 7 8 9]))
    (is (is-sorted-asc (insertion-sort (random-vec 1000 0 1000))))))
