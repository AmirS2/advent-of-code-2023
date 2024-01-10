(ns aoc2023.day-19.part1
  (:require [clojure.string :as str]))

(defn parse-rule [rule-str]
  (let [[_ xmas gtlt v target] (re-matches #"(?:([xmas])([<>])(\d+):)?(\w+)" rule-str)]
    {:attr xmas :gtlt gtlt :val (when v (Integer/parseInt v)) :target target}))

(defn get-workflow [line]
  (let [[_ wf-name rules-strs] (re-matches #"(\w+)\{(.*)\}" line)
        rules (map parse-rule (str/split rules-strs #","))]
    [wf-name rules]))

(defn get-part [line]
  (let [xmas (map #(str/split % #"=") (str/split (re-find #"[^{}]+" line) #","))]
    (into {} (map #(vector (first %) (Integer/parseInt (second %))) xmas))))

(defn process [[line & remaining] workflows]
  (if (str/blank? line)
    [workflows (vec (map get-part remaining))]
    (recur remaining (conj workflows (get-workflow line)))))

(defn get-wf-result [workflow part]
  (loop [[{:keys [attr gtlt val target]} & remaining] workflow]
    (if (or (not gtlt) ((case gtlt "<" < ">" >) (get part attr) val))
      target
      (recur remaining))))

(defn is-accepted [workflows]
  (fn [part]
    (loop [wf "in"]
      (let [next-wf (get-wf-result (get workflows wf) part)]
        (cond
          (= next-wf "A") true
          (= next-wf "R") false
          :else (recur next-wf))))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [[workflows parts] (process (line-seq rdr) {})
          accepted (filter (is-accepted workflows) parts)]
      (dorun (map println workflows))
      (dorun (map println parts))
      (println "Accepted:" (str/join "\n" accepted))
      (printf "Sum is: %d\n" (apply + (apply concat (map vals accepted))))
      ))
)
