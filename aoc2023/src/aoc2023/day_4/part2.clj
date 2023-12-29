(ns aoc2023.day-4.part2
  (:require [clojure.string :as str])
  (:require clojure.set))

(defn parse-line [line]
  (let [parts (str/split line #"[: |]+")
        parts-nums (vec (map #(Integer/parseInt %) (drop 1 parts)))]
    (hash-map
      :card-no (first parts-nums)
      :winning-nos (set (subvec parts-nums 1 11))
      :my-nos (set (subvec parts-nums 11)))))

(defn wins [card]
  (count (clojure.set/intersection (card :winning-nos) (card :my-nos))))

(defn calc-line-fn [cards]
  (fn [idx line]
    (let [card (parse-line line)
          score (wins card)
          reps (aget cards idx)]
      (doseq [x (range (+ idx 1) (+ idx score 1))]
        (aset-long cards x (+ (aget cards x) reps)))
      (println card score idx reps)
      reps
      )))

(defn process-lines [rdr]
  (let [cards (long-array (repeat 200 1))
        values (map-indexed (calc-line-fn cards) (line-seq rdr))]
    (reduce + 0 values)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (printf "Sum is: %d\n" (process-lines rdr)))
)
