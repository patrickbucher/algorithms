(ns algorithms.ch02.sorting-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.sorting :refer :all]))

(deftest test-equal
  (testing "equality of two vectors"
    (is (equal [] []))
    (is (not (equal [0] [])))
    (is (not (equal [] [0])))
    (is (equal [0] [0]))
    (is (equal [0 1 2] [0 1 2]))
    (is (not (equal [0 2 1] [0 1 2])))
    (is (not (equal [0 1 2 3] [0 1 2])))))

(deftest test-random-vec
  (testing "vector of given size with random elements within limits"
    (is (equal (random-vec 0 1 10) []))
    (is (equal (random-vec 10 10 1) []))
    (let [l (random-vec 1000 1 10)]
      (is (= (count l) 1000))
      (is (every? #(and (>= % 1) (< % 10)) l)))))

(deftest test-is-sorted
  (testing "order of elements in a vector"
    (let [asc #(<= %1 %2)
          desc #(>= %1 %2)]
      (is (is-sorted [] asc))
      (is (is-sorted [] desc))
      (is (is-sorted [0] asc))
      (is (is-sorted [0] desc))
      (is (is-sorted [0 1 2] asc))
      (is (not (is-sorted [0 1 2] desc)))
      (is (not (is-sorted [2 1 0] asc)))
      (is (is-sorted [2 1 0] desc))
      (is (is-sorted [0 1 2 3 4 5 6 7 8 9] asc))
      (is (not (is-sorted [0 1 2 3 4 5 6 7 8 9] desc)))
      (is (not (is-sorted [9 8 7 6 5 4 3 2 1 0] asc)))
      (is (is-sorted [9 8 7 6 5 4 3 2 1 0] desc))
      (is (not (is-sorted [5 4 6 3 7 2 1 8 9 0] asc)))
      (is (not (is-sorted [5 4 6 3 7 2 1 8 9 0] desc))))))

(deftest test-is-sorted-asc
  (testing "ascending order of elements in a vector"
    (is (is-sorted-asc []))
    (is (is-sorted-asc [0]))
    (is (is-sorted-asc [0 1]))
    (is (not (is-sorted-asc [1 0])))
    (is (is-sorted-asc [0 1 2]))
    (is (not (is-sorted-asc [0 2 1])))))

(deftest test-is-sorted-desc
  (testing "descending order of elements in a vector"
    (is (is-sorted-desc []))
    (is (is-sorted-desc [0]))
    (is (is-sorted-desc [1 0]))
    (is (not (is-sorted-desc [0 1])))
    (is (is-sorted-desc [2 1 0]))
    (is (not (is-sorted-desc [1 2 0])))))
