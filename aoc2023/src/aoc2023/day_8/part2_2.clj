(ns aoc2023.day-8.part2-2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

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

(defn get-steps [node-name data c]
  (let [instr-idx (mod c (:steps-count data))
        instruction (nth (:steps data) instr-idx)
        node ((:nodes data) node-name)
        next-node (node instruction)
        steps (+ c 1)]
    (if (= (nth next-node 2) \Z)
      steps
      (recur next-node data steps))))

(defn next-factor [n [d & factors]]
  (if (= (mod n d) 0)
    d
    (if (empty? factors)
      n
      (recur n factors))))

(defn prime-factors [n factors]
  (if (= n 1)
    factors
    (let [s (math/floor (math/sqrt n))
          d (next-factor n (cons 2 (range 3 s 2)))]
      (recur (/ n d) (conj factors d)))))

(defn count-steps [node-names data c]
  (seq (map #(get-steps % data 0) node-names)))

(defn starting-nodes [data]
  (filter #(= (nth % 2) \A) (keys (:nodes data))))

(defn common-factors [factor-frequencies]
  (loop [factors {}
         [next-freq & remaining] factor-frequencies]
    (let [new-factors (reduce-kv #(assoc %1 %2 (max %3 (get factors %2 0))) factors next-freq)]
      (if (empty? remaining)
        new-factors
        (recur new-factors remaining)))))

; Noticed that the end is reached after an exact number of loops of the instructions
(defn lcm [factors]
  (let [freqs (map frequencies factors)
        factors (common-factors freqs)
        factor-list (for [[factor freq] factors] (repeat freq factor))]
    (println factors freqs factor-list)
    (println (apply concat factor-list))
    (apply * (apply concat factor-list))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [data (parse (line-seq rdr))
          starting (doall (starting-nodes data))
          counts (count-steps starting data 0)
          factors (map #(prime-factors % []) counts)]
      (println data)
      (println starting)
      (printf "Steps count is: %s\n" counts)
      (printf "LCMs is: %d\n" (lcm factors))
      )
))
