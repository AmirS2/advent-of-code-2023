(ns aoc2023.day-9.part2
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map #(Integer/parseInt %) (str/split line #" +")))

(defn extrapolate [values acc minus]
  (let [next-acc ((if minus - +) acc (first values))]
    (if (apply = values)
      next-acc
      (recur (map - (rest values) (butlast values)) next-acc (not minus)))))

(defn extrapolate-lines [lines]
  (for [line lines]
    (let [values (parse-line line)]
      (extrapolate values 0 false))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [extrapolations (extrapolate-lines (line-seq rdr))]
      (println extrapolations)
      (printf "Sum is: %d\n" (apply + extrapolations))))
)
