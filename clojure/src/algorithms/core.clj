(ns algorithms.core
  (:require [clojure.math :refer [log10 pow sqrt]])
  (:gen-class))

(defn factorial
  "Computes the factorial of n."
  ([n] (factorial n 1))
  ([n acc]
   (if (= n 0)
     acc
     (factorial (dec n) (* n acc)))))

(def durations ["second" "minute" "hour" "day" "month" "year" "century"])
(def factors [1000000 60 60 24 30 12 100])
(def orders [["lg n" log10]
             ["√n" sqrt]
             ["n" #(%)]
             ["n lg n" #(* % (log10 %))]
             ["n²" #(pow % 2)]
             ["n³" #(pow % 3)]
             ["2^n" #(pow 2 %)]
             ["n!" factorial]])

(defn calculate-multipliers
  "Computes the milliseconds in durations with the factors provided."
  [durations factors]
  (into {} (map-indexed (fn [i d] [d (reduce * (take (inc i) factors))]) durations)))

(defn find-max-n
  "Computes the maximum value of n that can be computed within the limit."
  [f last-n n growth-f limit]
  (println f last-n n growth-f limit)
  (let [required (f n)]
    (if (< required limit)
      (find-max-n f n (growth-f n) growth-f limit)
      n)))

(defn -main
  [& args]
  (let [multipliers (calculate-multipliers durations factors)
        computations (for [o orders m multipliers] [o m])
        results (map (fn [[ord f] [dur ms]] (find-max-n f 0.0 10.0 #(* % 1.001) ms)) computations)]
    (apply println results)))
