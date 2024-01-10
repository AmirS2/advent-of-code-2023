(ns aoc2023.day-20.part1
  (:require [clojure.string :as str]))

(defn process [lines]
  ())

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [result (process (line-seq rdr))]
      (println result)
      (printf "Sum is: %d\n" (apply + result))
      ))
)
