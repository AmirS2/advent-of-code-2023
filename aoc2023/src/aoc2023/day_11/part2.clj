(ns aoc2023.day-11.part2
  (:require [clojure.string :as str]))

(defn empty-rows [galaxy]
  (keep-indexed (fn [idx row] (when (every? #(= \. %) row) idx)) galaxy))

(defn empty-cols [galaxy]
  (keep-indexed (fn [idx col] (when (every? #(= \. %) col) idx)) (apply map vector galaxy)))

(defn find-empty [galaxy]
  [(empty-rows galaxy) (empty-cols galaxy)])

(defn calc-coord [a empties]
  (apply + a (for [e empties :while (> a e)] 999999)))

(defn calc-coords [x y empty-rows empty-cols]
  [(calc-coord x empty-cols) (calc-coord y empty-rows)])

(defn find-stars [galaxy empty-rows empty-cols]
  (for [y (range (count galaxy))
        :let [galaxy-row (nth galaxy y)]
        x (range (count (first galaxy)))
        :let [p (nth galaxy-row x)]
        :when (= p \#)]
    (calc-coords x y empty-rows empty-cols)))

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
          [empty-rows empty-cols] (find-empty observed)
          stars (find-stars observed empty-rows empty-cols)
          distances (star-distances stars)]
      #_(doseq [row galaxy] (println (str/join row)))
      (println empty-rows)
      (println empty-cols)
      (println stars)
      (println distances)
      (printf "Sum is: %d\n" (apply + distances))
      ))
)
