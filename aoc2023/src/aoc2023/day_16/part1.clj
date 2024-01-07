(ns aoc2023.day-16.part1
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn process [lines]
  (let [grid (vec (map vec lines))]
    {:grid grid
     :lookup (fn [[x y]] ((grid y) x))
     :X (count (grid 0))
     :Y (count grid)}))

(defn addvec [a b] (vec (map + a b)))

(defn outside-grid [grid [x y]]
  (or (< x 0) (>= x (:X grid)) (< y 0) (>= y (:Y grid))))

(def dir-l [-1 0])
(def dir-r [1 0])
(def dir-u [0 -1])
(def dir-d [0 1])

(def lr #{dir-l dir-r})

(def ud #{dir-u dir-d})

(defn mirror-dir [dir p]
  (case p
    \\ (case dir
         [1 0] dir-d
         [0 1] dir-r
         [-1 0] dir-u
         [0 -1] dir-l)
    \/ (case dir
         [1 0] dir-u
         [0 -1] dir-r
         [-1 0] dir-d
         [0 1] dir-l)))

(defn new-visited [] {:l #{} :r #{} :u #{} :d #{}})

(def dir-keys {dir-l :l dir-r :r dir-u :u dir-d :d})

(defn add-visited [visited dir pos]
  (merge-with conj visited {(dir-keys dir) pos}))

(defn has-visited [visited dir pos]
  (((dir-keys dir) visited) pos))

(defn get-all-visited [{:keys [l r u d]}]
  (set/union l r u d))

(defn track-beams [grid [pos dir] remaining visited]
  (let [next-pos (addvec pos dir)
        new-visited (add-visited visited dir next-pos)
        outside (outside-grid grid next-pos)
        already-visited (has-visited visited dir next-pos)]
    (if (or outside already-visited)
      (if (not-empty remaining)
        (recur grid (first remaining) (rest remaining) visited)
        (get-all-visited visited))
      (let [p ((:lookup grid) next-pos)
            carry-on (or (= p \.) (and (= p \-) (lr dir)) (and (= p \|) (ud dir)))
            reflect (#{\\ \/} p)]
        (cond
          carry-on (recur grid [next-pos dir] remaining new-visited)
          reflect (recur grid [next-pos (mirror-dir dir p)] remaining new-visited)
          :split (let [splits (seq (if (= p \|) ud lr))
                       new-remaining (conj remaining [next-pos (second splits)])]
                   (recur grid [next-pos (first splits)] new-remaining new-visited)))))))

(defn visited-grid [grid visited]
  (vec (map-indexed (fn [y row]
                      (vec (map-indexed (fn [x c] (if (visited [x y]) \# c)) row))) grid)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [grid (process (line-seq rdr))
          start [-1 0]
          visited (track-beams grid [start dir-r] [] (new-visited))]
      (println grid)
      (println visited)
      (dorun (map #(println (str/join "" %)) (visited-grid (:grid grid) visited)))
      (printf "Sum is: %d\n" (count visited))
      ))
)
