(ns aoc2023.day-10.part1
  (:require [clojure.string :as str]))

(defn load-pipes [lines]
  (let [pipes-1 (for [line lines] (conj (vec (cons \. (seq line))) \.))
        empty-row (repeat (count (first pipes-1)) \.)
        pipes (conj (cons empty-row pipes-1) empty-row)]
    {:lookup (fn [[x y]] (nth (nth pipes (+ y 1)) (+ x 1)))
     :X (- (count (first pipes)) 2)
     :Y (- (count pipes) 2)}))

(defn find-start [pipes]
  (first (for [y (range (:Y pipes))
               x (range (:X pipes))
               :let [c ((:lookup pipes) [x y])]
               :when (= c \S)]
           [x y])))

(defn find-next-dir [[dx dy] pipe]
  (case pipe
    \- (if (= dx -1) [-1 0] [1 0])  ; continue l or r
    \F (if (= dx -1) [0 1] [1 0])   ; turn d or r
    \L (if (= dx -1) [0 -1] [1 0])  ; turn u or r
    \J (if (= dx 1) [0 -1] [-1 0])  ; turn u or l
    \7 (if (= dx 1) [0 1] [-1 0])   ; turn d or l
    \| (if (= dy 1) [0 1] [0 -1])   ; continue d or u
    ))

(defn do-find-loop [pipes prev-pos pos start c]
  (if (= pos start)
    c
    (let [pipe ((:lookup pipes) pos)
          dir (vec (map - pos prev-pos))
          next-dir (find-next-dir dir pipe)
          next-pos (vec (map + pos next-dir))]
      (recur pipes pos next-pos start (+ c 1)))))

(defn next-pos-from-start [pipes [x y]]
  (let [lookup (:lookup pipes)
        l [(- x 1) y]
        r [(+ x 1) y]
        u [x (- y 1)]
        d [x (+ y 1)]]
    (cond
      (#{\- \F \L} (lookup l)) l
      (#{\- \7 \J} (lookup r)) r
      (#{\| \7 \F} (lookup u)) u
      (#{\| \J \L} (lookup d)) d)))

(defn find-loop-size [pipes [x y]]
  (let [next-pos (next-pos-from-start pipes [x y])]
    (do-find-loop pipes [x y] next-pos [x y] 1)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [pipes (load-pipes (line-seq rdr))
          start (find-start pipes)
          loop-size (find-loop-size pipes start)]
      (println start)
      (println loop-size)
      (printf "Furthest is: %d\n" (/ loop-size 2))
      ))
)
