(ns aoc2023.day-9.part1
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (map #(Integer/parseInt %) (str/split line #" +")))

(defn extrapolate [values acc]
  (let [next-acc (+ (last values) acc)]
    (if (apply = values)
      next-acc
      (recur (map - (rest values) (butlast values)) next-acc))))

(defn extrapolate-lines [lines]
  (for [line lines]
    (let [values (parse-line line)]
      (extrapolate values 0))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [extrapolations (extrapolate-lines (line-seq rdr))]
      (println extrapolations)
      (printf "Sum is: %d\n" (apply + extrapolations))))
)
