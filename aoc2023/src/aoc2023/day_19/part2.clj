(ns aoc2023.day-19.part2
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

(defn get-wf-result [workflow rg]
  (loop [[{:keys [attr gtlt val target]} & remaining] workflow
         rg rg
         results []]
    (if (not gtlt)
      (conj results [rg target])
      (let [[attr-low attr-high] (get rg attr)]
        (println attr-low attr-high attr rg gtlt val target)
        (if (= gtlt "<")
          (cond
            (< attr-high val) (conj results [rg target])
            (<= val attr-low) (recur remaining rg results)
            :else (let [branch-range (merge rg {attr [attr-low (- val 1)]})
                        pass-range (merge rg {attr [val attr-high]})]
                    (recur remaining pass-range (conj results [branch-range target]))))
          (cond
            (<= attr-high val) (recur remaining rg results)
            (< val attr-low) (conj results [rg target])
            :else (let [branch-range (merge rg {attr [(+ val 1) attr-high]})
                        pass-range (merge rg {attr [attr-low val]})]
                    (recur remaining pass-range (conj results [branch-range target])))))))))

(defn get-accepted-ranges [ranges workflows accepted]
  (if (empty? ranges)
    accepted
    (let [[[rg next-wf] & remaining] ranges]
      (println "Ranges count:" (count ranges))
      (cond
        (= next-wf "A") (recur remaining workflows (conj accepted rg))
        (= next-wf "R") (recur remaining workflows accepted)
        :else (recur (concat remaining (get-wf-result (get workflows next-wf) rg)) workflows accepted)))))

(defn calc-combos [rg]
  (apply * (map #(- (apply - %) 1) (vals rg))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [[workflows parts] (process (line-seq rdr) {})
          accepted (get-accepted-ranges [[{"x" [1 4000] "m" [1 4000] "a" [1 4000] "s" [1 4000]} "in"]] workflows [])]
      (dorun (map println workflows))
      (println "Accepted:" (str/join "\n" accepted))
      (printf "Sum is: %d\n" (apply + (map calc-combos accepted)))
      ))
)
