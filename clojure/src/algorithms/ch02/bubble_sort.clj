(ns algorithms.ch02.bubble-sort)

(defn subvector
  "Returns a sub-vector v[i;j["
  [v i n]
  (cond (empty? v) []
        (< n 1) []
        (< i 0) []
        (< (count v) (+ i n)) []
        (= i 0) (vec (take n v))
        :else (recur (rest v) (dec i) n)))

(defn swap
  "Swaps the two elements at position i and j in v."
  [v i j]
  (let [n (count v)]
    (cond (or (neg? i) (neg? j)) v
          (= i j) v
          (> i j) (recur v j i)
          (or (>= i n) (>= j n)) v
          :else
          (let [x (nth v i)
                y (nth v j)
                l (subvector v 0 i)
                m (subvector v (inc i) (dec (- j i)))
                r (subvector v (inc j) (dec (- n j)))]
            (vec (flatten [l [y] m [x] r]))))))

(defn bubble-sort
  "Returns an ascendingly sorted vector by applying Bubble Sort to the given one."
  ([v]
   (let [n (count v)]
     (if (<= n 1) v (bubble-sort v n 0 (dec n)))))
  ([v n i j]
   (cond (= i (dec n)) v
         (= j i) (recur v n (inc i) (dec n))
         :else (let [r (nth v j)
                     l (nth v (dec j))]
                 (if (< r l)
                   (recur (swap v (dec j) j) n i (dec j))
                   (recur v n i (dec j)))))))
