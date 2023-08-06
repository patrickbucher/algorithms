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

(deftest test-binary-to-decimal
  (testing "little-endian binary to decimal conversion"
    (is (binary-to-decimal []) 0)
    (is (binary-to-decimal [0]) 0)
    (is (binary-to-decimal [1]) 1)
    (is (binary-to-decimal [0 1]) 2)
    (is (binary-to-decimal [1 1]) 3)
    (is (binary-to-decimal [0 0 1]) 4)
    (is (binary-to-decimal [1 0 1]) 5)
    (is (binary-to-decimal [0 1 1]) 6)
    (is (binary-to-decimal [1 1 1]) 7)
    (is (binary-to-decimal [0 0 0 1]) 8)))

(deftest test-decimal-to-binary
  (testing "decimal to little-endian binary conversion"
    (is (decimal-to-binary 0) [0])
    (is (decimal-to-binary 1) [1])
    (is (decimal-to-binary 2) [0 1])
    (is (decimal-to-binary 3) [1 1])
    (is (decimal-to-binary 4) [0 0 1])
    (is (decimal-to-binary 5) [1 0 1])
    (is (decimal-to-binary 6) [0 1 1])
    (is (decimal-to-binary 7) [1 1 1])
    (is (decimal-to-binary 8) [0 0 0 1])))
