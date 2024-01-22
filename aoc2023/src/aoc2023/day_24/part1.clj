(ns aoc2023.day-24.part1
  (:require [clojure.string :as str]))

(defn parse-hailstone [line]
  (let [[px py pz vx vy vz] (map #(Long/parseLong %) (str/split line #" ?[,@] +"))]
    {:pos [px py pz] :path [vx vy vz]}))

(defn process [lines]
  (vec (map parse-hailstone lines)))

; a + nv = b + mw
; (a - b) = (mw - nv)
;
; [ax-bx] = [wx  vx] x [m]
; [ay-by]   [wy  vy]   [-v]
;
; WV-1 x [ax-bx] = [m]
;        [ay-by]   [-v]
;
; 1/DET [vy  -vx]  x [ax-bx] = [m]
;       [-wy  wx]    [ay-by]   [-v]
;
; m = (vy * (ax-bx) - vx * (ay-by))/det
; P = b + mw
;   = [bx + (vy * (ax-bx) - vx * (ay-by))/det * wx]
;     [by + (vy * (ax-bx) - vx * (ay-by))/det * wy]

(defn check-parallel [h1 h2]
  (println "Parallel hailstones" h1 h2)
  false)

(defn check-collision-2d [{[ax ay az] :pos [vx vy vz] :path :as h1}
                          {[bx by bz] :pos [wx wy wz] :path :as h2}]
  (let [det (- (* wx vy) (* vx wy))]
  (if (= 0 det)  ; parallel
    (check-parallel h1 h2)
    (let [m (/ (- (* vy (- ax bx)) (* vx (- ay by))) det)
          n (/ (- (* wy (- ax bx)) (* wx (- ay by))) det)
          x (+ bx (* m wx))
          y (+ by (* m wy))]
      ;(and (<= 0 m) (<= 0 n) (<= 7 x) (<= x 27) (<= 7 y) (<= y 27))))))
      (and (<= 0 m) (<= 0 n) (<= 200000000000000 x) (<= x 400000000000000) (<= 200000000000000 y) (<= y 400000000000000))))))

(defn check-all-collisions [hailstones]
  (count (for [n (range (count hailstones))
               :let [h1 (hailstones n)]
               m (range (inc n) (count hailstones))
               :let [h2 (hailstones m)]
               :when (check-collision-2d h1 h2)]
           [h1 h2])))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [hailstones (process (line-seq rdr))]
      (println hailstones)
      (printf "Sum is: %d\n" (check-all-collisions hailstones))
      ))
)
