(ns aoc2023.day-10.part2
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

(defn do-find-loop [pipes prev-pos pos start positions]
  (if (= pos start)
    positions
    (let [pipe ((:lookup pipes) pos)
          dir (vec (map - pos prev-pos))
          next-dir (find-next-dir dir pipe)
          next-pos (vec (map + pos next-dir))]
      (recur pipes pos next-pos start (conj positions next-pos)))))

(defn next-pos-from-start [pipes [x y]]
  (let [lookup (:lookup pipes)
        l [(- x 1) y]
        r [(+ x 1) y]
        u [x (- y 1)]
        d [x (+ y 1)]
        has-l (#{\- \F \L} (lookup l))
        has-r (#{\- \7 \J} (lookup r))
        has-u (#{\| \7 \F} (lookup u))
        has-d (#{\| \J \L} (lookup d))
        next-pos (cond has-l l has-r r has-u u has-d d)
        start-pipe (cond has-l (cond has-r \- has-u \J has-d \7)
                         has-r (cond has-u \L has-d \F)
                         :else \|)]
    [next-pos start-pipe]))

(def inside-lines #{[\F \J] [\L \7] [\F \7] [\L \J]})
(def change-inside #{[\L \7] [\F \J] [\| \|] [\| \L] [\| \F]})

(defn get-spans [state [x pipe]]
  (println state x pipe)
  (if (empty? state)
    {:prev-x x :prev-pipe pipe :c 0 :inside false}
    (let [{:keys [prev-pipe prev-x c inside]} state
          new-inside (if (change-inside [prev-pipe pipe]) (not inside) inside)
          dc (if (inside-lines [prev-pipe pipe]) 0 (if new-inside (- x prev-x 1) 0))]
      {:prev-pipe pipe :prev-x x :c (+ c dc) :inside new-inside})))

(defn count-inside-row [sorted-xs pipes-row]
  (let [rel-xs (filter #(not (= (pipes-row %1) \-)) sorted-xs)
        _ (println rel-xs)
        pipes (map pipes-row rel-xs)
        _ (println pipes)
        spans (reduce get-spans {} (map vector rel-xs pipes))]
    (:c spans)))

(defn lookup-pipe-in-row [pipes y start-pipe]
  (fn [x]
    (let [pipe ((:lookup pipes) [x y])]
      (if (= pipe \S) start-pipe pipe))))

(defn count-inside-positions [edges pipes start-pipe]
  (let [miny (apply min (map second edges))
        maxy (apply max (map second edges))
        grouped-edges (group-by second edges)]
    (apply + (for [y (range miny (+ maxy 1))
                   :let [xs (sort (map first (grouped-edges y)))
                         pipes-row (lookup-pipe-in-row pipes y start-pipe)]]
               (do (println "Row: " y)
                   (count-inside-row xs pipes-row))))))

(defn find-loop-size [pipes [x y]]
  (let [[next-pos start-pipe] (next-pos-from-start pipes [x y])
        loop-positions (do-find-loop pipes [x y] next-pos [x y] #{next-pos})]
    (count-inside-positions loop-positions pipes start-pipe)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [pipes (load-pipes (line-seq rdr))
          start (find-start pipes)
          loop-size (find-loop-size pipes start)]
      (println start)
      (printf "Area is: %d\n" loop-size)
      ))
)
