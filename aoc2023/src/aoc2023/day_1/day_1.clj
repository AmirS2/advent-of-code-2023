(ns aoc2023.day-1.day-1)

(defn main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_1/input")]
    (doseq [line (line-seq rdr)]
      (println line)))
  )
