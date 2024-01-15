(ns aoc2023.day-20.part1
  (:require [clojure.string :as str]))

(defn parse-module [line]
  (let [[module outputs_str] (str/split line #" -> ")
        outputs (str/split outputs_str #", ")]
    (if (= module "broadcaster")
      ["broadcaster" {:type "b" :outputs outputs}]
      (let [mod-name (subs module 1)]
        [mod-name (case (first module)
                    \% {:type :flip :name mod-name :outputs outputs :state false}
                    \& {:type :conj :name mod-name :outputs outputs :inputs {}})]))))

(defn connect-inputs [cables [module-name & remaining]]
  (let [module (get cables module-name)
        dests (for [output (:outputs module)
                    :let [output-mod (get cables output)]
                    :when (= (:type output-mod) :conj)]
                [output (merge-with conj output-mod {:inputs {module-name :low}})])
        new-dests (into {} dests)
        new-cables (merge cables new-dests)]
    (if (empty? remaining)
      new-cables
      (recur new-cables remaining))))

(defn process [lines]
  (->> (map parse-module lines)
       (into {})))

(defn send-outputs [m sign]
  (map #(list % sign (:name m)) (:outputs m)))

(defn broadcast [b sign]
  [b (send-outputs b sign)])

(defn flip-flop [f sign]
  (case sign
    :high [f []]
    :low (let [state (not (:state f))]
           [(assoc f :state state) (send-outputs f (if state :high :low))])))

(defn conjunction [c sign from]
  (let [new-inputs (assoc (:inputs c) from sign)
        new-c (assoc c :inputs new-inputs)
        all-high (every? (partial = :high) (vals new-inputs))]
    [new-c (send-outputs c (if all-high :low :high))]))

(defn count-pulses [cables [[pulse-name sign from] & to-process] lows highs]
  (let [this-mod (get cables pulse-name {:type :output})
        new-lows (case sign :low (inc lows) :high lows)
        new-highs (case sign :high (inc highs) :low highs)
        [new-mod pulses] (case (:type this-mod)
                           "b" (broadcast this-mod sign)
                           :flip (flip-flop this-mod sign)
                           :conj (conjunction this-mod sign from)
                           :output [this-mod []])
        new-process (concat to-process pulses)]
    (if (empty? new-process)
      [cables new-lows new-highs]
      (recur (assoc cables pulse-name new-mod) new-process new-lows new-highs))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [result (process (line-seq rdr))
          cables (connect-inputs result (keys result))]
      (dorun (map println cables))
      (loop [c 1000
             [cables l h] [cables 0 0]]
        (println c l h cables)
        (if-not (zero? c)
          (recur (dec c) (count-pulses cables [["broadcaster" :low "button"]] l h))
          (printf "Product is: %d\n" (* l h)))
      )))
)
