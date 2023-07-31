(ns algorithms.core
  (:gen-class))

(def durations ["second" "minute" "hour" "day" "month" "year" "century"])
(def factors [1000000 60 60 24 30 12 100])

(defn calculate-multipliers
  "Computes the milliseconds in durations with the factors provided."
  [durations factors]
  (into {} (map-indexed (fn [i d] [d (reduce * (take (inc i) factors))]) durations)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
