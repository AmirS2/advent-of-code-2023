(ns aoc2023.day-1.part2
  (:require [clojure.string :as str]))

(def numbers
  {"one" 1
   "two" 2
   "three" 3
   "four" 4
   "five" 5
   "six" 6
   "seven" 7
   "eight" 8
   "nine" 9})

(def numbers-rev
  (zipmap (map (comp str/reverse name) (keys numbers)) (vals numbers)))

(def numbers-re
  (re-pattern (format "(?<digit>\\d)|(?<text>%s)"
                      (str/join "|" (map #(format "%s" %)
                                         (keys numbers))))))

(def numbers-re-rev
  (re-pattern (format "(?<digit>\\d)|(?<text>%s)"
                      (str/join "|" (map #(format "%s" (str/reverse %))
                                         (keys numbers))))))

(defn digit [matcher lookup]
  (let [digit (.group matcher "digit")
        text (.group matcher "text")]
    (if (str/blank? text)
      (Integer/parseInt digit)
      (get lookup text))))

(defn parse_line
  [line]
  (let [matcher (re-matcher numbers-re line)
        match (re-find matcher)
        first_dig (digit matcher numbers)
        rev-matcher (re-matcher numbers-re-rev (str/reverse line))
        match-rev (re-find rev-matcher)
        last_dig (digit rev-matcher numbers-rev)]
        (+ (* first_dig 10) last_dig)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_1/input")]
    (doseq [line (line-seq rdr)] (println (parse_line line) line)))
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_1/input")]
    (printf "Result is: %d\n"
            (reduce #(+ %1 (parse_line %2)) 0 (line-seq rdr))))
)
