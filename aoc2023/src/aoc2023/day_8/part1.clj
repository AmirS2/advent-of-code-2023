(ns aoc2023.day-8.part1
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

(defn count-steps [node-name data c]
  (let [instr-idx (mod c (:steps-count data))
        instruction (nth (:steps data) instr-idx)
        node ((:nodes data) node-name)
        next-node (node instruction)
        steps (+ c 1)]
    (println "Step: " instruction next-node steps)
    (if (= next-node "ZZZ")
      steps
      (recur next-node data steps))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [data (parse (line-seq rdr))]
      (println data)
      (printf "Steps count is: %d\n" (count-steps "AAA" data 0))
      )
))
