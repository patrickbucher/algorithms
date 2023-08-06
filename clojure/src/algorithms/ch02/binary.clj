(ns algorithms.ch02.binary)

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
