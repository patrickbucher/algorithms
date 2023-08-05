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
