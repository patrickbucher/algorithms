(ns algorithms.ch02.sorting)

(defn hello [what]
  (str "hello, " what))

(defn equal
  "Returns true if the vectors are equal, false otherwise."
  [left right]
  (cond (and (empty? left) (empty? right)) true
        (not (= (count left) (count right))) false
        (= (first left) (first right)) (equal (rest left) (rest right))
        :else false))

(defn random-list
  "Returns a list of size n with random elements in [min;max[."
  ([n min max]
   (if (>= min max)
     []
     (random-list n min max '())))
  ([n min max acc]
   (if (= n 0)
     acc
     (random-list (dec n) min max (cons (+ (rand-int (- max min)) min) acc)))))
