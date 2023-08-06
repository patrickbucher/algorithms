(ns algorithms.ch02.binary-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.binary :refer :all]
            [algorithms.ch02.sorting :refer :all]))


(deftest test-add-binary
  (testing "adding little-endian binaries"
    (is (equal (add-binary [] []) [0]))
    (is (equal (add-binary [0] [1]) [1 0]))
    (is (equal (add-binary [0 1 0 1] [1 0 1 1]) [1 1 1 0 1]))
    (is (equal (add-binary [0 1 1 1] [1 0 1 1]) [1 1 0 1 1]))))
