(ns algorithms.ch02.linear-search-test
  (:require [clojure.test :refer :all]
            [algorithms.ch02.linear-search :refer :all]))

(deftest test-linear-search
  (testing "linear search in sorted vector"
    (is (= (linear-search [] 0) -1))
    (is (= (linear-search [0] 0) 0))
    (is (= (linear-search [0 1 2 3] 3) 3))
    (is (= (linear-search [0 1 2 3] 0) 0))
    (is (= (linear-search [0 1 2 3] 4) -1))
    (is (= (linear-search (take 100 (iterate inc 0)) 99) 99))
    (is (= (linear-search (take 100 (iterate inc 0)) 100) -1))))
