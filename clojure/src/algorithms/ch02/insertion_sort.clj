(ns algorithms.ch02.insertion-sort)

(defn insert-into
  "Inserts item in ascending order into coll."
  [item coll]
  (let [left (take-while #(< % item) coll)
        right (drop (count left) coll)]
    (flatten [left item right])))

(defn insertion-sort
  "Returns a vector sorted ascendingly containing the elements of the
  given vector."
  ([v] (insertion-sort [] v))
  ([left right]
   (if (empty? right)
     left
     (let [item (first right)
           left (insert-into item left)
           right (rest right)]
       (insertion-sort left right)))))

