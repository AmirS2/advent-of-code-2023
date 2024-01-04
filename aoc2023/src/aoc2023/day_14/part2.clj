(ns aoc2023.day-14.part2
  (:require [clojure.string :as str]))

(defn read-field [lines]
  (vec (map vec lines)))

(defn merge-row [row to-merge]
  (let [row-pairs (for [i (range (count row))
                        :let [r (nth row i)
                              m (nth to-merge i)]]
                    (if (and (= r \.) (= m \O)) [m r] [r m]))]
    (apply map vector row-pairs)))

(defn do-roll [curr next-row]
  (if (empty? curr)
    [next-row]
    (let [last-row (last curr)
          merged-rows (merge-row last-row next-row)]
      (vec (concat (butlast curr) merged-rows)))))

(defn print-field [field]
  (dorun (map (comp println str/join) field))
  (println))

(defn roll-n [field]
  (reduce do-roll [] field))

(defn roll-n-fully [field]
  (loop [prev-field nil
         curr-field field]
    (if (= prev-field curr-field)
      curr-field
      (recur curr-field (roll-n curr-field)))))

(defn rotate-90-c [field]
  (vec (apply map (comp vec reverse vector) field)))

(defn cycle-field [field]
  (-> field
      roll-n-fully
      rotate-90-c
      roll-n-fully
      rotate-90-c
      roll-n-fully
      rotate-90-c
      roll-n-fully
      rotate-90-c))

(defn load-n [field]
  (let [c (count field)]
    (apply + (for [y (range c)]
               (* (- c y) (count (filter #(= \O %) (nth field y))))))))

(defn calc-end-load [idx loads]
  (let [curr (count loads)
        cycle-length (- curr idx)
        remaining (- 1000000000 curr)
        leftover (mod remaining cycle-length)
        end-load (nth loads (+ idx leftover))]
    (printf "Curr: %d\nCycle-length: %d\nRemaining: %d\nLeftover: %d\n"
            curr cycle-length remaining leftover)
    end-load))

(defn find-final-load [field]
  (loop [prev-fields []
         prev-loads []
         field field]
    (let [prev-match-idx (keep-indexed #(when (= field %2) %1) prev-fields)]
      (if (first prev-match-idx)
        (calc-end-load (first prev-match-idx) prev-loads)
        (let [curr-load (load-n field)
              next-field (cycle-field field)]
          (println (count prev-loads) curr-load)
          (recur (conj prev-fields field) (conj prev-loads curr-load) next-field))))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [field (read-field (line-seq rdr))
          final-load (find-final-load field)]
      (println "Load is: " final-load)
      ))
)
