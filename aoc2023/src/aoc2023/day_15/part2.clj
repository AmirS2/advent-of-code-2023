(ns aoc2023.day-15.part2
  (:require [clojure.string :as str]))

(defn do-hash [h c]
  (mod (* (+ h (int c)) 17) 256))

(defn calc-hash [s]
  (reduce do-hash 0 s))

(defn get-step [s]
  (let [[label lens-str] (str/split s #"[-=]")
        label-hash (calc-hash label)
        lens (when lens-str (Integer/parseInt lens-str))]
    {:label label :hash label-hash :lens lens}))

(defn get-steps [line]
  (seq (map get-step (str/split line #","))))

(defn process-box [box step]
  (let [{:keys [lens label]} step]
    (if lens
      (if (some #(= label (:label %)) box)
        (map #(if (= label (:label %)) step %) box)
        (conj box step))
      (filter #(not (= label (:label %))) box))))

(defn process-step [boxes step]
  (let [box-no (:hash step)
        box (nth boxes box-no)
        new-box (vec (process-box box step))]
    (assoc boxes box-no new-box)))

(defn process-steps [steps]
  (let [boxes (vec (repeat 256 []))]
    (reduce process-step boxes steps)))

(defn box-power [box]
  (apply + (map-indexed (fn [j lens] (* (+ 1 j) (:lens lens))) box)))

(defn focusing-power [idx box]
  (* (+ 1 idx) (box-power box)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [steps (get-steps (first (line-seq rdr)))
          boxes (process-steps steps)]
      (println steps)
      (dorun (map-indexed #(println %1 %2) boxes))
      (printf "Sum is: %d\n" (apply + (map-indexed focusing-power boxes)))
      ))
)
