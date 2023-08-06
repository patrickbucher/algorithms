(ns algorithms.ch02.sorting)

(defn equal
  "Returns true if the vectors are equal, false otherwise."
  [left right]
  (cond (and (empty? left) (empty? right)) true
        (not (= (count left) (count right))) false
        (= (first left) (first right)) (equal (rest left) (rest right))
        :else false))

(defn random-vec
  "Returns a vector of size n with random elements in [min;max[."
  [n min max]
  (if (>= min max)
    []
    (vec (take n (drop 1 (iterate (fn [_] (+ (rand-int (- max min)) min)) 0))))))

(defn is-sorted
  "Returns true if the given vector is sorted in order of the predicate
  p, false otherwise."
  [v p]
  (if (or (empty? v) (= (count v) 1))
    true
    (and (p (first v) (second v)) (is-sorted (rest v) p))))
