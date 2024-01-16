(ns aoc2023.day-21.part1
  (:require [clojure.string :as str]))

(defn process [lines]
  (vec (map vec lines)))

(defn print-map [garden]
  (dorun (map (comp println #(str/join "" %)) garden)))

(defn find-start [garden]
  (first (for [y (range (count garden))
               x (range (count (garden 0)))
               :when (= \S ((garden y) x))]
           [x y])))

(defn find-moves [[x y] garden]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [xx (+ x dx)
              yy (+ y dy)]
        :when (and (>= xx 0) (>= yy 0) (< xx (count (garden 0))) (< yy (count garden))
                   (#{\. \S} ((garden yy) xx)))]
    [xx yy]))

(defn find-positions [steps garden reached]
  (println steps (count reached))
  (let [new-reached (loop [[pos & remaining] reached
                           new-reached #{}]
                      (let [next-new-reached (apply conj new-reached (find-moves pos garden))]
                        (if (empty? remaining)
                          next-new-reached
                          (recur remaining next-new-reached))))]
    (if (> steps 1)
      (recur (dec steps) garden new-reached)
      new-reached)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [garden (process (line-seq rdr))
          start (find-start garden)
          reached (find-positions 64 garden #{start})]
      (print-map garden)
      (println start)
      (println reached)
      (println (count reached))
      #_(printf "Sum is: %d\n" (apply + result))
      ))
)
