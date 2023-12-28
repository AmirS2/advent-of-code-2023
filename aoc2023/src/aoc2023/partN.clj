(ns aoc2023.day-m.partN
  (:require [clojure.string :as str]))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (doseq [line (line-seq rdr)]
      (println line)))
      #_(printf "Sum is: %d\n" (apply + gear-powers))
)
