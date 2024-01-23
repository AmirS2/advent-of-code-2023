(ns aoc2023.day-24.part2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn parse-hailstone [line]
  (let [[px py pz vx vy vz] (map #(Long/parseLong %) (str/split line #" ?[,@] +"))]
    {:pos [px py pz] :path [vx vy vz]}))

(defn process [lines]
  (vec (map parse-hailstone lines)))

; a + tu = r + ts
; b + t2v = r + t2s
; c + t3w = r + t3s
;
; (a - r) = t(s - u)
; (b - r) = t2(s - v)
; (c - r) = t3(s - w)
;
; (ax - rx)/(sx - ux) = (ay - ry)/(sy - uy) = (az - rz)/(sz - uz) = t
; (ax - rx) = (ay - ry) * (sx - ux) / (sy - uy)
; rx = ax - (ay - ry) * (sx - ux) / (sy - uy)

(defn calc-offset [[bx by bz] [ex ey ez] {[hx hy hz] :pos [vx vy vz] :path}]
  (let [dx (- ex hx)
        dy (- ey hy)
        dz (- ez hz)]
    (+ (* bx (- (* dy vz) (* dz vy)))
       (* by (- (* dz vx) (* dx vz)))
       (* bz (- (* dx vy) (* dy vx))))))

(defn calc-total-offset [b e hailstones]
  (println b e)
  (apply + (for [h hailstones]
             (calc-offset b e h))))

(defn calc-delta [b e hailstones]
  (let [base (calc-total-offset b e hailstones)
        dx (calc-total-offset (map + b [1 0 0]) e hailstones)
        dy (calc-total-offset (map + b [0 1 0]) e hailstones)
        dz (calc-total-offset (map + b [0 0 1]) e hailstones)
        di (calc-total-offset b (map + e [1 0 0]) hailstones)
        dj (calc-total-offset b (map + e [0 1 0]) hailstones)
        dk (calc-total-offset b (map + e [0 0 1]) hailstones)]
    [(map (partial - base) [dx dy dz]) (map (partial - base) [di dj dk])]))

(defn find-intersect-line [b e hailstones]
  (let [o (calc-total-offset b e hailstones)]
    (if (zero? o)
      [b e]
      (let [[db de] (calc-delta b e hailstones)
            bb (map + b (map #(int (/ o (* 2 %))) db))
            ee (map + e (map #(int (/ o (* 3 %))) de))]
        (println "Recur" bb ee o db de)
        (recur bb ee hailstones)))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [hailstones (process (line-seq rdr))]
      (println hailstones)
      (find-intersect-line [100 200 300] [400 500 600] hailstones)
      #_(printf "Sum is: %d\n" (check-all-collisions hailstones))
      ))
)
