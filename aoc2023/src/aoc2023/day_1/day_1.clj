(ns aoc2023.day-1.day-1
  (:require [clojure.string :as str]))

(defn first_digit [s] (re-find #"\d" s))

(defn last_digit [s] (re-find #"\d" (str/reverse s)))

(defn parse_line
  [line]
  (+ (* (Integer/parseInt (first_digit line)) 10)
     (Integer/parseInt (last_digit line))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_1/input")]
    (doseq [line (line-seq rdr)] (println (parse_line line))))
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_1/input")]
    (printf "Result is: %d\n"
            (reduce #(+ %1 (parse_line %2)) 0 (line-seq rdr))))
)
