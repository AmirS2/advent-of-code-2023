(ns aoc2023.day-18.part2
  (:require [clojure.string :as str]))

(def directions {"0" [1 0] "2" [-1 0] "1" [0 1] "3" [0 -1]})

(defn get-dig [line]
  (let [[_ dist_h dir] (re-find #"#(\p{XDigit}{5})(.)" line)
        dist (Integer/parseInt dist_h 16)]
    [dist dir]))

(defn get-edges [[line & remaining] [x y] edges edge_length]
  (let [[dist dir] (get-dig line)
        dir_vec (directions dir)
        ;new_edges (rest (reductions #(map + %1 %2) [x y] (repeat dist dir_vec)))
        new_edge (map + [x y] (map * [dist dist] dir_vec))
        updated_edges (conj edges new_edge)
        new_edge_length (+ edge_length dist)]
    (if (empty? remaining)
      [updated_edges new_edge_length]
      (recur remaining new_edge updated_edges new_edge_length))))

(defn shoelace [edges]
  (/ (apply + (map (fn [[a1 a2] [b1 b2]] (- (* a1 b2) (* a2 b1))) (butlast edges) (rest edges))) 2))

(defn process [lines]
  (let [[edges edge_length] (get-edges lines [0 0] [[0 0]] 0)
        area (shoelace edges)]
  (+ area 1 (/ edge_length 2))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [points (process (line-seq rdr))]
      (println points)
      (printf "Area is: %d\n" points)
      ))
)
