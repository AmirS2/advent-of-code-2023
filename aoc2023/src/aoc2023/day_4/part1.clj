(ns aoc2023.day-4.part1
  (:require [clojure.string :as str])
  (:require clojure.set))

(defn parse-line [line]
  (let [parts (str/split line #"[: |]+")
        parts-nums (vec (map #(Integer/parseInt %) (drop 1 parts)))]
    (hash-map
      :card-no (first parts-nums)
      :winning-nos (set (subvec parts-nums 1 11))
      :my-nos (set (subvec parts-nums 11)))))

(defn worth [card]
  (let [wins (count (clojure.set/intersection (card :winning-nos) (card :my-nos)))]
    (if (>= wins 1)
      (Math/pow 2 (- wins 1))
      0)))

(defn process-line [line]
  (worth (parse-line line)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [total-worth (reduce + 0 (map process-line (line-seq rdr)))]
      (printf "Sum is: %f\n" total-worth)))
)
