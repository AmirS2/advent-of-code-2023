(ns aoc2023.day-8.part2
  (:require [clojure.string :as str]))

(defn parse-node [line]
  (let [[name left right] (str/split line #"[ =(,)]+")]
    {:name name \L left \R right}))

(defn parse-nodes [nodes-lines]
  (let [nodes (map parse-node nodes-lines)]
    (zipmap (map :name nodes) nodes)))

(defn parse [[steps-lines _ & nodes-lines]]
  (let [steps (seq steps-lines)
        nodes (parse-nodes nodes-lines)]
    {:steps steps :nodes nodes :steps-count (count steps)}))

(defn count-steps [node-names data c]
  (let [instr-idx (mod c (:steps-count data))
        instruction (nth (:steps data) instr-idx)
        nodes (doall (map (:nodes data) node-names))
        next-nodes (doall (map #(get % instruction) nodes))
        steps (+ c 1)]
    (when (= (mod c 100000) 0) (println instruction steps next-nodes))
    (if (empty? (doall (filter #(not (= (nth % 2) \Z)) next-nodes)))
      steps
      (recur next-nodes data steps))))

(defn starting-nodes [data]
  (filter #(= (nth % 2) \A) (keys (:nodes data))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [data (parse (line-seq rdr))
          starting (doall (starting-nodes data))]
      (println data)
      (println starting)
      (printf "Steps count is: %d\n" (count-steps starting data 0))
      )
))
