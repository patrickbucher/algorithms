(ns algorithms.ch02.sorting-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.sorting :refer :all]))

(deftest some-test
  (testing "everything works"
    (is (= (hello "world") "hello, world"))))

(deftest test-equal
  (testing "equality of two vectors"
    (is (equal [] []))
    (is (not (equal [0] [])))
    (is (not (equal [] [0])))
    (is (equal [0] [0]))
    (is (equal [0 1 2] [0 1 2]))
    (is (not (equal [0 2 1] [0 1 2])))
    (is (not (equal [0 1 2 3] [0 1 2])))))

(deftest test-random-list
  (testing "list of given size with random elements within limits"
    (is (equal (random-list 0 1 10) []))
    (is (equal (random-list 10 10 1) []))
    (let [l (random-list 1000 1 10)]
      (is (= (count l) 1000))
      (is (every? #(and (>= % 1) (< % 10)) l)))))
