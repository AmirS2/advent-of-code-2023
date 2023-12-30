(ns aoc2023.day-6.part1
  (:require [clojure.string :as str]))

(defn parse-nos [line]
  (map #(Integer/parseInt %) (rest (str/split line #" +"))))

(defn parse-input [[times distances]]
  (map #(hash-map :time %1 :distance %2)
       (parse-nos times)
       (parse-nos distances)))

(defn ways-count [{:keys [time distance]}]
  (count (filter #(> (* (- time %) %) distance) (range time))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [races (parse-input (line-seq rdr))
          ways (for [race races
                     :let [ways (ways-count race)]]
                 (do (println race ways)
                      ways))]
      (printf "Product is: %d\n" (apply * ways))
      ))
)
