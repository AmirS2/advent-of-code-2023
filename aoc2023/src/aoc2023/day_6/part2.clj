(ns aoc2023.day-6.part2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn parse-nos [line]
  (Long/parseLong (str/join (rest (str/split line #" +")))))

(defn parse-input [[times distances]]
  {:time (parse-nos times) :distance (parse-nos distances)})

(defn ways-count "t * t - time * t + distance = 0" [{:keys [time distance]}]
  (let [b (- time)
        c distance
        b2 (* b b)
        ac4 (* 4 c)
        s (math/sqrt (- b2 ac4))
        start (/ (- (+ b s)) 2)
        end (/ (- s b) 2)]
    [start end]))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [race (parse-input (line-seq rdr))
          rg (ways-count race)]
      (println race)
      (printf "Range is: %f %f\n" (first rg) (second rg))
      (printf "Ways is: %f\n" (- (math/floor (second rg)) (math/floor (first rg)))))
      )
)
