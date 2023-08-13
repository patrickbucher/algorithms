(ns algorithms.ch02.linear-search)

(defn linear-search
  ([xs x] (linear-search xs x 0))
  ([xs x i]
   (if (empty? xs)
     -1
     (if (= (first xs) x)
       i
       (linear-search (rest xs) x (inc i))))))
