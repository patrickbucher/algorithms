(ns algorithms.ch02.bubble-sort-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.sorting :refer :all]
            [algorithms.ch02.bubble-sort :refer :all]))

(deftest test-bubble-sort
  (testing "bubble sort"
    (is (is-sorted-asc (bubble-sort [])))
    (is (is-sorted-asc (bubble-sort [1])))
    (is (is-sorted-asc (bubble-sort [1 2 3 4 5])))
    (is (is-sorted-asc (bubble-sort [5 1 3 2 4])))
    (is (equal (bubble-sort [9 1 8 2 7 3 6 4 5 0]) [0 1 2 3 4 5 6 7 8 9]))
    (is (is-sorted-asc (bubble-sort (random-vec 100 0 1000))))))
