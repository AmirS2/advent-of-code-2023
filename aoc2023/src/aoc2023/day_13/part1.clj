(ns aoc2023.day-13.part1
  (:require [clojure.string :as str]))

(defn parse-fields [lines]
  (loop [field []
         fields []
         [line & remaining] lines]
    (if (str/blank? line)
      (recur [] (conj fields field) remaining)
      (if (empty? remaining)
        (conj fields (conj field line))
        (recur (conj field line) fields remaining)))))

(defn find-horiz-mirror [field]
  (let [sz (count field)
        _ (println sz)
        mirror-pos (for [i (range 1 sz)
                         :let [c (min i (- sz i))
                               below (take c (nthrest field i))
                               above (take c (reverse (take i field)))]
                         :when (= above below)]
                     i)]
    (get (vec mirror-pos) 0 0)))

(defn find-vert-mirror [field]
  (find-horiz-mirror (apply map vector field)))

(defn find-mirror [field]
  (+ (find-vert-mirror field) (* 100 (find-horiz-mirror field))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [fields (parse-fields (line-seq rdr))
          mirrors (map find-mirror fields)]
      (println fields)
      (println mirrors)
      (printf "Sum is: %d\n" (apply + mirrors))
      ))
)
