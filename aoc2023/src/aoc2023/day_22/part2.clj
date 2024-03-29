(ns aoc2023.day-22.part2
  (:require [clojure.string :as str]))

(defn read-brick [line]
  (let [[s e] (str/split line #"~")
         pc (fn [l] (map #(Integer/parseInt %) (str/split l #",")))
         [sx sy sz] (pc s)
         [ex ey ez] (pc e)]
    {:x [sx ex] :y [sy ey] :z (vec (sort [sz ez]))}))

(defn process [lines]
  (map read-brick lines))

(defn sort-bricks [bricks]
  (vec (sort-by #(max (:z %)) bricks)))

(defn overlap [[al ah] [bl bh]]
  (and (<= al bh) (<= bl ah)))

(defn intersect [brick b]
  (and
    (overlap (:z brick) (:z b))
    (overlap (:y brick) (:y b))
    (overlap (:x brick) (:x b))))

(defn fits [brick lower-bricks]
  (if (empty? lower-bricks) true
    (loop [[b & remaining] lower-bricks]
      (cond
        (intersect brick b) false
        (empty? remaining) true
        :else (recur remaining)))))

(defn drop-brick [brick lower-bricks]
  (last (for [z (range (first (:z brick)) 0 -1)
              :let [test-brick (assoc brick :z [z (- z (apply - (:z brick)))])]
              :while (fits test-brick lower-bricks)]
          test-brick)))

(defn settle [bricks n c]
  (if (= n (count bricks))
    [bricks c]
    (let [brick (nth bricks n)
          new-brick (drop-brick brick (take n bricks))
          new-c (if (= new-brick brick) c (inc c))
          new-bricks (assoc bricks n new-brick)]
      (recur new-bricks (inc n) new-c))))

(defn get-intersections [b bricks above]
  (let [b-up (assoc b :z (vec (map (if above inc dec) (:z b))))]
    (filter #(intersect b-up %) bricks)))

(defn is-disintegratable [brick above same-level]
  (not-any? #(= 1 (count (get-intersections % same-level false)))
            (get-intersections brick above true)))

(defn find-disintegratable [bricks]
  (let [bricks-by-lower-level (group-by (comp first :z) bricks)
        bricks-by-higher-level (group-by (comp second :z) bricks)]
    (for [b bricks
          :let [above (->> b :z second inc (get bricks-by-lower-level))
                same-level (->> b :z second (get bricks-by-higher-level))]
          :when (is-disintegratable b above same-level)]
      b)))

(defn count-drops [bricks]
  (apply
    +
    (for [n (range (count bricks))
          :let [sb (into (subvec bricks 0 n) (subvec bricks (inc n)))
                [_ c] (settle sb 0 0)]]
      (do (println c)
      c))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [result (process (line-seq rdr))
          sorted-bricks (sort-bricks result)
          [settled, _] (settle sorted-bricks 0 0)
          drops (count-drops (sort-bricks settled))]
      #_(println sorted-bricks)
      (dorun (map println settled))
      (printf "Count is: %d\n" drops)
      ))
)
