(ns algorithms.ch02.selection-sort)

(defn smallest-to-front
  "Returns v with the smallest element in front."
  ([v]
   (if (empty? v)
     v
     (smallest-to-front [] (first v) (rest v))))
  ([skipped smallest remainder]
   (if (empty? remainder)
     (flatten [[smallest] skipped])
     (let [current (first remainder)
           remainder (rest remainder)]
       (if (< current smallest)
         (smallest-to-front (conj skipped smallest) current remainder)
         (smallest-to-front (conj skipped current) smallest remainder))))))

(defn selection-sort
  "Returns a vector sorted ascendingly containint the elements of the
  given vector."
  ([v]
   (if (empty? v)
     v
     (selection-sort v [])))
  ([v acc]
   (if (empty? v)
     acc
     (let [right (smallest-to-front v)
           smallest (first right)
           remainder (rest right)]
       (selection-sort remainder (conj acc smallest))))))

