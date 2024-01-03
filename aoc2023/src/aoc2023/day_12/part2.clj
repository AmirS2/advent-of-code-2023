(ns aoc2023.day-12.part2
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

(defn do-count-combos [{:keys [springs groups]} memoized-count-combos]
  (if (empty? groups)
    (if (every? #(#{\? \.} %) springs) 1 0)
    (let [group (first groups)]
      ;(println groups)
      (apply
        +
        (for [group-pos (find-possible-group-positions springs group)
              :let [next-springs (nthrest springs (+ group group-pos 1))
                    next-record {:springs next-springs :groups (rest groups)}
                    next-combos (memoized-count-combos next-record memoized-count-combos)]
              :when (pos-int? next-combos)]
          next-combos)))))

(def memoized-count-combos (memoize do-count-combos))

(defn count-combos [record]
  (println "Calculating for: " record)
  (let [c (memoized-count-combos record memoized-count-combos)]
    (println "Count:" c)
    c))

(defn expand-records [{:keys [springs groups]}]
  (let [springs (apply concat (conj (repeat 4 (conj springs \?)) springs))
        groups (apply concat (repeat 5 groups))]
   {:springs springs :groups groups}))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [condition-records (vec (map parse-record (line-seq rdr)))
          expanded-records (map expand-records condition-records)
          combos (vec (map count-combos expanded-records))]
      (printf "Sum is: %d\n" (apply + combos))
      ))
)
