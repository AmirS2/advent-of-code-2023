(ns aoc2023.day-19.part1
  (:require [clojure.string :as str]))

(defn parse-rule [rule-str]
  (let [[_ xmas gtlt v target] (re-matches #"(?:([xmas])([<>])(\d+):)?(\w+)" rule-str)]
    {:attr xmas :gtlt gtlt :val v :target target}))

(defn get-workflow [line]
  (let [[_ wf-name & rules-strs] (re-matches #"(\w+)\{(?:([^,]*),)*(.*)\}" line)
        rules (map parse-rule rules-strs)]
    [wf-name rules]))

(defn get-parts [lines]
  lines)

(defn process [[line & remaining] workflows]
  (if (str/blank? line)
    [workflows (get-parts remaining)]
    (recur remaining (conj workflows (get-workflow line)))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [result (process (line-seq rdr) {})]
      (println result)
      #_(printf "Sum is: %d\n" (apply + result))
      ))
)
