(ns algorithms.core
  (:require [clojure.math :refer [log10 pow sqrt]])
  (:gen-class))

(defn factorial
  "Computes the factorial of n."
  ([n] (factorial n 1))
  ([n acc]
   (if (< n 1)
     acc
     (recur (dec n) (* n acc)))))

(def durations ["second" "minute" "hour" "day" "month" "year" "century"])
(def factors [1000000 60 60 24 30 12 100])
(def orders [["lg n" log10]
             ["√n" sqrt]
             ["n" identity]
             ["n lg n" #(* % (log10 %))]
             ["n²" #(pow % 2)]
             ["n³" #(pow % 3)]
             ["2^n" #(pow 2 %)]
             ["n!" #(factorial (int %))]])

(defn calculate-multipliers
  "Computes the milliseconds in durations with the factors provided."
  [durations factors]
  (into {} (map-indexed (fn [i d] [d (reduce * (take (inc i) factors))]) durations)))

(defn find-max-n
  "Computes the maximum value of n that can be computed within the limit."
  [ord f last-n n growth-f limit]
  (let [required (f n)]
    (if (< required limit)
      (recur ord f n (growth-f n) growth-f limit)
      last-n)))

(defn -main
  [& args]
  (let [multipliers (calculate-multipliers durations factors)
        computations (for [o orders m multipliers] [o m])
        results (map (fn [[[ord f] [dur ms]]]
                       [ord dur (find-max-n ord f 0.0 10.0 #(* % 1.001) ms)])
                     computations)]
    (apply println (map #(format "%s %s: n=%.3e\n" (nth % 0) (nth % 1) (nth % 2)) results))))
