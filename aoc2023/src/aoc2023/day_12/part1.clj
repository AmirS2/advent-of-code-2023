(ns aoc2023.day-12.part1
  (:require [clojure.string :as str]))

(defn parse-record [line]
  (let [[springs groups-str] (str/split line #" ")
        groups (map #(Integer/parseInt %) (str/split groups-str #","))]
    {:springs (seq springs) :groups groups}))

(defn is-possible-group-posn [springs group position]
  (let [prev (take position springs)
        g (take group (nthrest springs position))
        nxt (nth springs (+ position group) \.)]
  (and (every? #(#{\? \.} %) prev)
       (every? #(#{\? \#} %) g)
       (#{\? \.} nxt))))

(defn find-possible-group-positions [springs group]
  (for [x (range (+ (- (count springs) group) 1))
        :when (is-possible-group-posn springs group x)]
    x))

(defn do-count-combos [{:keys [springs groups]}]
  (if (empty? groups)
    (if (every? #(#{\? \.} %) springs) 1 0)
    (let [group (first groups)]
      (println groups)
      (apply
        +
        (for [group-pos (find-possible-group-positions springs group)
              :let [_ (println "Testing" group "at" group-pos)
                    next-springs (nthrest springs (+ group group-pos 1))
                    next-record {:springs next-springs :groups (rest groups)}
                    next-combos (do-count-combos next-record)]
              :when (pos-int? next-combos)]
          next-combos)))))

(defn count-combos [record]
  (println "Calculating for: " record)
  (let [c (do-count-combos record)]
    (println "Count:" c)
    c))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [condition-records (vec (map parse-record (line-seq rdr)))
          combos (vec (map count-combos condition-records))]
      (printf "Sum is: %d\n" (apply + combos))
      ))
)
