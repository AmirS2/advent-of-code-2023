(ns aoc2023.day-20.part1
  (:require [clojure.string :as str]))

(defn parse-module [line]
  (let [[module outputs_str] (str/split line #" -> ")
        outputs (str/split outputs_str #", ")]
    (if (= module "broadcaster")
      ["broadcaster" {:type "b" :outputs outputs}]
      [(subs module 1) {:type (first module) :outputs outputs}])))

(defn connect-inputs [cables]


(defn process [lines]
  (->> (map parse-module lines)
       (into {})))

(defn count-pulses [cables [[pulse-name sign] & to-process] lows highs]
  (let [next-mod (get cables pulse-name)]
    (case (:type next-mod)
      "b" (recur cables (concat to-process (map #(list % sign) (:outputs next-mod))) 

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [result (process (line-seq rdr))]
      (println result)
      #_(printf "Sum is: %d\n" (apply + result))
      ))
)
