(ns algorithms.ch02.binary-search-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.binary-search :refer :all]))

(deftest test-binary-search
  (testing "binary search in sorted vector"
    (is (= (binary-search [] 0) -1))
    (is (= (binary-search [0] 0) 0))
    (is (= (binary-search [0 1 2 3] 3) 3))
    (is (= (binary-search [0 1 2 3] 0) 0))
    (is (= (binary-search [0 1 2 3] 4) -1))
    (is (= (binary-search (take 100 (iterate inc 0)) 99) 99))
    (is (= (binary-search (take 100 (iterate inc 0)) 100) -1))))
