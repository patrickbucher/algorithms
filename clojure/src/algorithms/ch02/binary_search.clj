(ns algorithms.ch02.binary-search)

(defn binary-search
  ([xs x] (binary-search xs x 0 (count xs)))
  ([xs x l u]
   (if (or (empty? xs) (>= l u))
     -1
     (let [n (- u l)
           m (+ (int (/ n 2)) l)
           e (nth xs m)]
       (cond (< n 1) -1
             (= e x) m
             (< x e) (binary-search xs x l m)
             (> x e) (binary-search xs x (inc m) u))))))

