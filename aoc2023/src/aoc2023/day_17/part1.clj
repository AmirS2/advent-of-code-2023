(ns aoc2023.day-17.part1
  (:require [clojure.string :as str]))

(defn process-city [lines]
  (let [city (vec (map (fn [line] (vec (map #(- (int %) (int \0)) line))) lines))]
    {:city city :X (count (city 0)) :Y (count city) :lookup (fn [[x y]] ((city y) x))}))

(defn new-lowest-temps [x y]
  (let [ks (for [xx (range x)
                 yy (range y)
                 dir [:l :r :u :d]
                 step [0 1 2]]
             [[xx yy] dir step])]
    (zipmap ks (repeat 999999))))

(defn get-turns [dir]
  (case dir
    (:l :r) [[:u 0] [:d 0]]
    (:u :d) [[:l 0] [:r 0]]))

(defn take-step [pos dir]
  (vec (map + pos (case dir :l [-1 0] :r [1 0] :u [0 -1] :d [0 1]))))

(defn can-step [city [x y]]
  (and (>= x 0) (>= y 0) (< x (:X city)) (< y (:Y city))))

; update format: [[pos dir steps] loss]
(defn potential-updates [city update-list]
  (apply
    concat
    (for [[[pos dir steps] loss] update-list
          :let [turns (get-turns dir)
                all-steps (if (< steps 2) (conj turns [dir (+ steps 1)]) turns)]]
      (for [step all-steps
            :let [new-pos (take-step pos (first step))]
            :when (can-step city new-pos)]
        [[new-pos (first step) (second step)] (+ loss ((:lookup city) new-pos))]))))

(defn create-update-map [[[k loss] & remaining] update-map]
  (let [current (get update-map k 999999)
        updated-loss (min loss current)
        updated-map (assoc update-map k updated-loss)]
    (if (not-empty remaining)
      (recur remaining updated-map)
      updated-map)))

(defn do-merge-updates [[[k loss] & remaining] lowest-temps updated]
  (let [current (lowest-temps k)
        is-update (< loss current)
        new-lowest-temps (if is-update (assoc lowest-temps k loss) lowest-temps)
        new-updated (if is-update (conj updated [k loss]) updated)]
    (if (empty? remaining)
      [new-updated new-lowest-temps]
      (recur remaining new-lowest-temps new-updated))))

(defn merge-updates [update-list lowest-temps]
  (do-merge-updates (create-update-map update-list {}) lowest-temps []))

(defn update-city [city update-list lowest-temps]
  (let [next-update-list (potential-updates city update-list)
        [next-updates new-lowest-temps] (merge-updates next-update-list lowest-temps)]
    (println "Iterating with" (count update-list) "updates")
    (if (empty? next-updates)
      (let [pos [(- (:X city) 1) (- (:Y city) 1)]]
        (println lowest-temps)
        (apply min (for [dir [:l :r :u :d] step [0 1 2]]
                     (get lowest-temps [pos dir step]))))
      (recur city next-updates new-lowest-temps))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [city (process-city (line-seq rdr))
          start [[[[0 0] :d 0] 0] [[[0 0] :d 0] 0]]
          lowest (new-lowest-temps (:X city) (:Y city))
          upd-lowest (assoc lowest (first start) (second start))
          min-heat-loss (update-city city start upd-lowest)]
      (println city)
      (printf "Sum is: %d\n" min-heat-loss)
      ))
)
