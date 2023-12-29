(ns aoc2023.day-5.part2
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
  (let [[_ & seed_strs] (str/split line #" ")
        seed-vals (map #(Long/parseLong %) seed_strs)]
    (loop [[x y & remaining] seed-vals
           seed-pairs []]
      (let [new-seed-pairs (conj seed-pairs {:from x :to (+ x y)})]
        (if (empty? remaining) new-seed-pairs (recur remaining new-seed-pairs))))))

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

(defn between [value start end]
  (and (<= start value) (< value end)))

(defn split-seeds [{seed-from :from seed-to :to} mapping]
  (let [within-seed #(between % seed-from seed-to)
        splits (filter within-seed (set (concat (map :source-from mapping)
                                                (map :source-to mapping))))
        sorted-splits (vec (sort splits))]
    (println seed-from seed-to sorted-splits)
    (map #(hash-map :from %1 :to %2)
         (cons seed-from sorted-splits)
         (conj sorted-splits seed-to))))

(defn lookup-seed "Lookup seed range, returns a list of mapped ranges"
  [seed-range {mapping :vals}]
  (let [seed-ranges (split-seeds seed-range mapping)]
    (println "Split ranges: " seed-range seed-ranges)
    (for [{seed-from :from seed-to :to} seed-ranges]
      (let [mapped-offset (for [{:keys [dest-from source-from source-to]} mapping
                              :when (between seed-from source-from source-to)]
                            (- dest-from source-from))
            offset (if (empty? mapped-offset) 0 (first mapped-offset))]
        {:from (+ seed-from offset) :to (+ seed-to offset)}))))

(defn get-end-loc [seeds [next_map & remaining]]
  (let [next-seeds (flatten (for [seed seeds] (lookup-seed seed next_map)))]
    (if (empty? remaining) next-seeds (recur next-seeds remaining))))

(defn parse-lines [[first-line & remaining]]
  (let [seeds (parse-seeds first-line)
        ;[see-soi soi-fer fer-wat wat-lig lig-tem tem-hum hum-loc]
        maps (parse-maps remaining)]
    (println seeds)
    (get-end-loc seeds maps)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [data (parse-lines (line-seq rdr))]
      (printf "Final locs are: %s\n" (vec data))
      (printf "Closest loc is %s\n" (apply min (map :from data)))
      ))
)
