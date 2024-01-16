(ns aoc2023.day-21.part2
  (:require [clojure.string :as str]))

(defn process [lines]
  (let [garden (vec (map vec lines))
        x (count (garden 0))
        y (count garden)]
  {:garden garden :X x :Y y :get (fn [xx yy] ((garden (mod yy y)) (mod xx x)))}))

(defn print-map [garden]
  (dorun (map (comp println #(str/join "" %)) (:garden garden))))

(defn print-reached [garden reached]
  (for [y (range (:Y garden))]
    (let [row (for [x (range (:X garden))]
                (if (reached [x y]) \O ((:get garden) x y)))]
      (println (str/join "" row)))))

(defn find-start [garden]
  (first (for [y (range (:Y garden))
               x (range (:X garden))
               :when (= \S ((:get garden) x y))]
           [x y])))

(defn find-moves [[x y] garden]
  (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
        :let [xx (+ x dx)
              yy (+ y dy)]
        :when (#{\. \S} ((:get garden) xx yy))]
    [xx yy]))

(defn is-pattern [counts period]
  (if (> (count counts) (+ 5 (* period 2)))
    (let [diffs (map - (nthnext counts period) (take (- (count counts) period) counts))
          ddiffs (map - (nthnext diffs period) (take (- (count diffs) period) diffs))
          ]
      (if (apply = (take-last period ddiffs))
        (do (println (count counts))
            (println counts)
            (println diffs)
            (println ddiffs)
            true)
        false))))

(defn find-positions [steps garden reached counts]
  #_(dorun (print-reached garden reached))
  (println steps (count reached))
  (let [new-reached (loop [[pos & remaining] reached
                           new-reached #{}]
                      (let [next-new-reached (apply conj new-reached (find-moves pos garden))]
                        (if (empty? remaining)
                          next-new-reached
                          (recur remaining next-new-reached))))
        count-reached (count new-reached)
        new-counts (conj counts count-reached)]
    (if-not (is-pattern new-counts (:X garden))
      (recur (inc steps) garden new-reached new-counts)
      new-counts)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [garden (process (line-seq rdr))
          start (find-start garden)
          reached (find-positions 0 garden #{start} [0])
          period (:X garden)
          steps 26501365
          offset (- (quot (count reached) period) 3)
          step-periods (- (quot steps period) offset)
          stub (+ (mod steps period) (* period offset))
          l (nth reached stub)
          l2 (nth reached (+ stub period))
          l3 (nth reached (+ stub period period))
          diff (- l2 l)
          diff2 (- l3 l2)
          plots (+ l (* step-periods diff) (/ (* step-periods (- step-periods 1) (- diff2 diff)) 2))]
      (println step-periods stub l l2 l3 diff diff2)
      (println plots)
      #_(printf "Sum is: %d\n" (apply + result))
      ))
)
