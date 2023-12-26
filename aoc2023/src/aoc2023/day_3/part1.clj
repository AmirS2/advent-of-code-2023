(ns aoc2023.day-3.part1
  (:require [clojure.string :as str]))

(defn is-digit [c]
  (and (<= (int \0) (int c)) (>= (int \9) (int c))))

(defn is-symbol [c]
  (and (not (= \. c)) (not (is-digit c))))

(defn is-part [schematic y x size]
  (let [upper-row (take (+ size 2) (nthrest (schematic (- y 1)) (- x 1)))
        lower-row (take (+ size 2) (nthrest (schematic (+ y 1)) (- x 1)))]
    (or (some is-symbol upper-row)
        (some is-symbol lower-row)
        (is-symbol ((schematic y) (- x 1)))
        (is-symbol ((schematic y) (+ x size))))))

(defn get-parts [schematic]
  (let [rows (count schematic)
        cols (count (schematic 0))]
    (for [y (range 1 (- rows 1))
          :let [row (schematic y)]
          x (range 1 (- cols 1))
          :let [prev (row (- x 1))
                cur (row x)]
          :when (and (not (is-digit prev)) (is-digit cur))
          :let [digits (take-while is-digit (nthrest row x))
                size (count digits)]
          :when (is-part schematic y x size)]
      (reduce (fn [c n] (+ (* 10 c) (- (int n) (int \0)))) 0 digits))))

(defn pad-line [line]
  (vec (cons \. (conj (vec line) \.))))

(defn pad-schematic [schematic]
  (let [cols (count (schematic 0))
        pad-line (repeat cols \.)]
    (vec (cons pad-line (conj schematic pad-line)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_3/input")]
    (let [schematic_cent (vec (map pad-line (line-seq rdr)))
          schematic (pad-schematic schematic_cent)
          parts (seq (get-parts schematic))]
      (printf "Parts are: %s\n" parts)
      (printf "Sum is: %d\n" (apply + parts))))
)
