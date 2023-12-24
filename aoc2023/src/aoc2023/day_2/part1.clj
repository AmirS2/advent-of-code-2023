(ns aoc2023.day-2.part1
  (:require [clojure.string :as str]))

(def reveal_re #"(\d+) (red|green|blue)(, (\d+) (red|green|blue))?(, (\d+) (red|green|blue))?")

(def default_reveal {"red" 0 "blue" 0 "green" 0})

(defn parse_reveal [reveal]
  (let [matches (re-find reveal_re reveal)]
    (merge default_reveal
           {(nth matches 2) (Integer/parseInt (nth matches 1))}
           (if (nth matches 5) {(nth matches 5) (Integer/parseInt (nth matches 4))} {})
           (if (nth matches 8) {(nth matches 8) (Integer/parseInt (nth matches 7))} {}))))

(defn parse_line [line]
  (let [games (re-find #"^Game (\d+): (.*)" line)
        game_no (second games)
        reveals (str/split (nth games 2) #"; ")]
    {:game-no (Integer/parseInt game_no)
     :reveals (map parse_reveal reveals)}))

(defn test_reveal [reveal]
  (and (<= (reveal "red") 12) (<= (reveal "green") 13) (<= (reveal "blue") 14)))

(defn pass_reveals [reveals]
  (every? test_reveal reveals))

(defn pass_game [game]
  (if (pass_reveals (game :reveals))
    (game :game-no)
    0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_2/input")]
    (doseq [line (line-seq rdr)] (println line) (println (parse_line line))))
  (with-open [rdr (clojure.java.io/reader "src/aoc2023/day_2/input")]
    (printf "Result is: %d\n"
            (reduce + 0 (map
                          (comp pass_game parse_line)
                          (line-seq rdr)))))
)
