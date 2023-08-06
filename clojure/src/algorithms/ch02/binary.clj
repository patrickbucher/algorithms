(ns algorithms.ch02.binary
  (:require [clojure.math :refer [pow]]))

(defn add-binary
  "Adds two binary numbers represented as little endian collections of
  zeros and ones."
  ([as bs]
   (if (not (= (count as) (count bs)))
     []
     (add-binary as bs [] 0)))
  ([as bs cs acc]
   (if (or (empty? as) (empty? bs))
     (conj cs acc)
     (let [a (first as)
           b (first bs)
           c (+ a b acc)
           as (rest as)
           bs (rest bs)]
       (cond (or (= c 0) (= c 1)) (add-binary as bs (conj cs c) 0)
             (= c 2) (add-binary as bs (conj cs 0) 1)
             (= c 3) (add-binary as bs (conj cs 1) 1))))))

(defn binary-to-decimal
  "Converts a little-endian binary number to its decimal representation."
  ([bin] (if (empty? bin)
           [0]
           (binary-to-decimal bin 0 0)))
  ([bin i acc]
   (if (empty? bin)
     acc
     (let [b (first bin)
           bin (rest bin)]
       (recur bin (inc i) (+ acc (int (* b (pow 2 i)))))))))

(defn decimal-to-binary
  "Converts a decimal number to its little-endian binary represenation."
  ([x] (if (<= x 1)
         [0]
         (decimal-to-binary x [])))
  ([x acc]
   (if (= x 0)
     acc
     (let [d (mod x 2)
           x (int (/ x 2))
           acc (conj acc d)]
       (recur x acc)))))
