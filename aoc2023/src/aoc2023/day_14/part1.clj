(ns aoc2023.day-14.part1
  (:require [clojure.string :as str]))

(defn read-field [lines]
  (vec (map vec lines)))

(defn merge-row [row to-merge]
  (let [row-pairs (for [i (range (count row))
                        :let [r (nth row i)
                              m (nth to-merge i)]]
                    (if (and (= r \.) (= m \O)) [m r] [r m]))]
    (apply map vector row-pairs)))

(defn do-roll-n [curr next-row]
  (if (empty? curr)
    [next-row]
    (let [last-row (last curr)
          merged-rows (merge-row last-row next-row)]
      (vec (concat (butlast curr) merged-rows)))))

(defn print-field [field]
  (dorun (map (comp println str/join) field))
  (println))

(defn roll-n [field]
  (reduce do-roll-n [] field))

(defn load-n [field]
  (let [c (count field)]
    (apply + (for [y (range c)]
               (* (- c y) (count (filter #(= \O %) (nth field y))))))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [field (read-field (line-seq rdr))
          n-field (nth (iterate roll-n field) (count field))]
      (print-field n-field)
      (printf "Load is: %d\n" (load-n n-field))
      ))
)
