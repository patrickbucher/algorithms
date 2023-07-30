(ns algorithms.core
  (:gen-class))

(def durations ["second" "minute" "hour" "day" "month" "year" "century"])
(def factors [1000000 60 60 24 30 12 100])

(defn calculate-multipliers
  "Computes the milliseconds in durations with the factors provided."
  [durations factors]
  (let [duration-factors (partition 2 (interleave durations factors))]
    (reduce (fn [{millis :millis durations :durations}
                 pair]
              (let [duration (first pair)
                    multiplier (last pair)
                    millis (* millis multiplier)]
                {:millis millis
                 :durations (conj durations (list duration millis))}))
            {:millis 1 :durations (list)}
            duration-factors)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
