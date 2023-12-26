(ns aoc2023.day-3.part2
  (:require [clojure.string :as str]))

(defn is-digit [c]
  (and (<= (int \0) (int c)) (>= (int \9) (int c))))

(defn is-gear [c]
  (= \* c))

(defn get-gears [schematic yy xx size]
  (for [y (range (- yy 1) (+ yy 2))
        x (range (- xx 1) (+ xx size 1))
        :when (is-gear ((schematic y) x))]
    (list y x)))

(defn get-number-from-digits [digits]
  (reduce (fn [c n] (+ (* 10 c) (- (int n) (int \0)))) 0 digits))

(defn get-digits-gears [schematic]
  (let [rows (count schematic)
        cols (count (schematic 0))]
    (for [y (range 1 (- rows 1))
          :let [row (schematic y)]
          x (range 1 (- cols 1))
          :let [prev (row (- x 1))
                cur (row x)]
          :when (and (not (is-digit prev)) (is-digit cur))
          :let [digits (take-while is-digit (nthrest row x))
                size (count digits)
                number (get-number-from-digits digits)]
          gear_position (get-gears schematic y x size)]
      (list gear_position number))))

(defn pad-line [line]
  (vec (cons \. (conj (vec line) \.))))

(defn pad-schematic [schematic]
  (let [cols (count (schematic 0))
        pad-line (vec (repeat cols \.))]
    (vec (cons pad-line (conj schematic pad-line)))))

(defn add-gear-pair [gears-map new-gear-pair]
  (let [gear_pos (first new-gear-pair)
        gear_val (second new-gear-pair)
        gear_list (get gears-map gear_pos [])]
    (assoc gears-map gear_pos (cons gear_val gear_list))))

(defn find-gear-powers [gears-numbers]
  (let [gear-map (reduce add-gear-pair {} gears-numbers)]
    (for [gear-info gear-map
          :let [gear_pos (first gear-info)
                gears (second gear-info)]
          :when (= 2 (count gears))]
      (* (first gears) (second gears)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_3/input")]
    (let [schematic_cent (vec (map pad-line (line-seq rdr)))
          schematic (pad-schematic schematic_cent)
          gears-numbers (seq (get-digits-gears schematic))
          gear-powers (find-gear-powers gears-numbers)]
      (printf "Gears are: %s\n" gears-numbers)
      (printf "Gears powers are: %s\n" (seq gear-powers))
      (printf "Sum is: %d\n" (apply + gear-powers))))
)
