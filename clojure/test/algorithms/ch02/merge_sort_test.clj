(ns algorithms.ch02.merge-sort-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.sorting :refer :all]
            [algorithms.ch02.merge-sort :refer :all]))

(deftest test-merge-sort
  (testing "merge sort"
    (is (is-sorted-asc (merge-sort [])))
    (is (is-sorted-asc (merge-sort [1])))
    (is (is-sorted-asc (merge-sort [1 2 3 4 5])))
    (is (is-sorted-asc (merge-sort [5 1 3 2 4])))
    (is (equal (merge-sort [9 1 8 2 7 3 6 4 5 0]) [0 1 2 3 4 5 6 7 8 9]))
    (is (is-sorted-asc (merge-sort (random-vec 1000 0 1000))))))

(deftest test-parallel-merge-sort
  (testing "parallel merge sort"
    (is (is-sorted-asc (parallel-merge-sort [])))
    (is (is-sorted-asc (parallel-merge-sort [1])))
    (is (is-sorted-asc (parallel-merge-sort [1 2 3 4 5])))
    (is (is-sorted-asc (parallel-merge-sort [5 1 3 2 4])))
    (is (equal (parallel-merge-sort [9 1 8 2 7 3 6 4 5 0]) [0 1 2 3 4 5 6 7 8 9]))
    (is (is-sorted-asc (parallel-merge-sort (random-vec 1000 0 1000))))))

