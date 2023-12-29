(ns aoc2023.day-5.part1
  (:require [clojure.string :as str]))

(defn parse-chunk-name [line]
  (first (str/split line #" ")))

(defn parse-chunk-values [line]
  (let [[dest source size] (map #(Long/parseLong %) (str/split line #" "))]
    {:dest-from dest :source-from source :source-to (+ source size)}))

(defn parse-chunk [[chunk-name & chunk-vals]]
  {:name (parse-chunk-name chunk-name)
   :vals (map parse-chunk-values chunk-vals)})

(defn parse-seeds [line]
  (let [[_ & seed_strs] (str/split line #" ")]
    (map #(Long/parseLong %) seed_strs)))

(defn do-split-chunks [[next_line & remaining] current_chunk chunks]
  (if next_line
    (if (= next_line "")
      (recur remaining [] (if (empty? current_chunk) chunks (conj chunks current_chunk)))
      (recur remaining (conj current_chunk next_line) chunks))
    (conj chunks current_chunk)))

(defn split-chunks [lines]
  (do-split-chunks lines [] []))

(defn parse-maps [lines]
  (map parse-chunk (split-chunks lines)))

(defn lookup-seed [seed {mapping :vals}]
  (let [mapped-seed (for [{:keys [dest-from source-from source-to]} mapping
                          :when (and (<= source-from seed) (< seed source-to))]
                      (+ dest-from (- seed source-from)))]
    (if (empty? mapped-seed) seed (first mapped-seed))))

(defn get-end-loc [seed [next_map & remaining]]
  (let [next-val (lookup-seed seed next_map)]
    (if (empty? remaining) next-val (recur next-val remaining))))

(defn parse-lines [[first-line & remaining]]
  (let [seeds (parse-seeds first-line)
        ;[see-soi soi-fer fer-wat wat-lig lig-tem tem-hum hum-loc]
        maps (parse-maps remaining)]
    (println seeds)
    (for [seed seeds]
      (get-end-loc seed maps))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [data (parse-lines (line-seq rdr))]
      (printf "Final locs are: %s\n" (vec data))
      (printf "Closest loc is %s\n" (apply min data))
      ))
)
