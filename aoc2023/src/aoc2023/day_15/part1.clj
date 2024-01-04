(ns aoc2023.day-15.part1
  (:require [clojure.string :as str]))

(defn do-hash [h c]
  (mod (* (+ h (int c)) 17) 256))

(defn calc-hash [s]
  (reduce do-hash 0 s))

(defn hash-line [line]
  (map calc-hash (str/split line #",")))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [hashes (hash-line (first (line-seq rdr)))]
      (println hashes)
      (printf "Sum is: %d\n" (apply + hashes))
      ))
)
