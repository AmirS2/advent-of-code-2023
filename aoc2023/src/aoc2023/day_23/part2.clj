(ns aoc2023.day-23.part2
  (:require [clojure.string :as str]))

(defn process [lines]
  (vec (map vec lines)))

(defn opp [dir]
  (case dir
    \^ \v
    \< \>
    \> \<
    \v \^))

(defn mov [dir]
  (case dir
    \^ [0 -1]
    \v [0 1]
    \< [-1 0]
    \> [1 0]))

(defn directions [trail-map [pos dir]]
  (for [d (seq [\^ \v \< \>])
        :let [[x y] (map + pos (mov d))]
        :when (and (not (= dir (opp d)))
                   (< x (count (trail-map 0)))
                   (>= x 0)
                   (< y (count trail-map))
                   (>= y 0)
                   (not (= ((trail-map y) x) \#)))]
    [[x y] d]))

(defn find-trail [trail-map [pos dir] c]
  (let [next-positions (directions trail-map [pos dir])]
    (case (count next-positions)
      0 [pos c []]
      1 (recur trail-map (first next-positions) (inc c))
      (2 3) [pos c next-positions])))

(defn find-trails [trail-map [[next-step dir] & remaining] trail-net]
  (let [[end-trail steps next-trails] (find-trail trail-map [next-step dir] 1)
        next-steps (if (contains? trail-net end-trail) remaining (concat remaining next-trails))
        prev-step (vec (map + next-step (mov (opp dir))))
        new-trail-net (assoc-in trail-net [prev-step end-trail] steps)]
    (if (empty? next-steps)
      new-trail-net
      (recur trail-map next-steps new-trail-net))))

(defn find-longest [trail-net]
 (let [f (fn [pos visited memoised]
           (apply max 0 (for [[end steps] (filter (comp not visited first) (get trail-net pos))]
                           (+ steps (memoised end (conj visited end) memoised)))))
       mem-f (memoize f)
       do-find-longest #(mem-f % #{} mem-f)]
   (- (do-find-longest [1 -1]) 1)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [trail-map (process (line-seq rdr))
          trail-net (find-trails trail-map [[[1 0] \v]] {})]
      (dorun (map #(println (str/join "" %)) trail-map))
      (dorun (map println trail-net))
      (printf "Longest is: %d\n" (find-longest trail-net))
      ))
)
