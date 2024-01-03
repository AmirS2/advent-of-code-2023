(ns aoc2023.day-11.part1
  (:require [clojure.string :as str]))

(defn expand-row [row]
  (if (every? #(= \. %) row)
    [row row]
    [row]))

(defn expand-col [& col]
  (expand-row col))

(defn expand-rows [galaxy]
  (apply concat (map expand-row galaxy)))

(defn expand-cols [galaxy]
  (apply map list (apply concat (apply map expand-col galaxy))))

(defn expand-galaxy [galaxy]
  (-> galaxy expand-rows expand-cols))

(defn find-stars [galaxy]
  (for [y (range (count galaxy))
        :let [galaxy-row (nth galaxy y)]
        x (range (count (first galaxy)))
        :let [p (nth galaxy-row x)]
        :when (= p \#)]
    [x y]))

(defn star-distance [a b]
  (+ (abs (- (first a) (first b))) (abs (- (second a) (second b)))))

(defn star-distances [[star & remaining]]
  (if (empty? remaining) []
    (concat (map #(star-distance star %) remaining) (star-distances remaining))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [observed (seq (line-seq rdr))
          galaxy (expand-galaxy observed)
          stars (find-stars galaxy)
          distances (star-distances stars)]
      #_(doseq [row galaxy] (println (str/join row)))
      (printf "Sum is: %d\n" (apply + distances))
      ))
)
