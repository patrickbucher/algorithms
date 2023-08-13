(ns algorithms.ch02.merge-sort)

(defn merge-ordered
  "Merges the given ordered vectors into a single ordered vector."
  ([l r] (merge-ordered l r []))
  ([l r acc]
   (cond (and (empty? l) (empty? r)) acc
         (empty? l) (flatten [acc r])
         (empty? r) (flatten [acc l])
         :else (let [a (first l)
                     b (first r)
                     nl (rest l)
                     nr (rest r)]
                 (if (<= a b)
                   (recur nl r (conj acc a))
                   (recur l nr (conj acc b)))))))

(defn merge-sort
  "Returns an ascendingly sorted vector."
  [v]
  (let [n (count v)]
    (if (<= n 1)
      v
      (let [m (int (/ n 2))
            l (merge-sort (subvec v 0 m))
            r (merge-sort (subvec v m n))]
        (merge-ordered l r)))))

(defn parallel-merge-sort
  "Returns an ascendingly sorted vector (done concurrently)."
  [v]
  (let [n (count v)]
    (if (<= n 1)
      v
      (let [m (int (/ n 2))
            l (future (merge-sort (subvec v 0 m)))
            r (future (merge-sort (subvec v m n)))]
        (merge-ordered @l @r)))))
