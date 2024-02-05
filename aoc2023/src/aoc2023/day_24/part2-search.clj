(ns aoc2023.day-24.part2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn vec-len [v]
  (math/sqrt (apply + (map #(* % %) v ))))

(defn parse-hailstone [line]
  (let [[px py pz vx vy vz] (map #(Double/parseDouble %) (str/split line #" ?[,@] +"))]
    {:pos [px py pz] :path [vx vy vz] :len (vec-len [vx vy vz])}))

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
; rx = ax + (ry - ay) * (sx - ux) / (sy - uy)

; (ax - rx) * (sy - uy) - (ay - ry) * (sx - ux) = l = 0
; (bx - rx) * (sy - vy) - (by - ry) * (sx - vx) = l2 = 0
; (cx - rx) * (sy - wy) - (cy - ry) * (sx - wx) = l3 = 0
;
; dl/drx = - (sy - uy)
; dl/dry = (sx - ux)
; dl/dsy = (ax - rx)
; dl/dsx = - (ay - ry)

; ax + tux = rx + tsx
; bx + t2vx = rx + t2sx
; cx + t3wx = rx + t3sx
;
; rx = ax + t(ux - sx)
; bx + t2(vx - sx) = ax + t(ux - sx)
;
; t = (ry - ay) / (uy - sy)
;
; Dist between b and a at time T = (b + Tv) - (a + Tu)
; particle r leaves a at time T
; catches b at time T2. must go at speed = (b + T2v)

(defn calc-offset [[bx by bz] [ex ey ez] {[hx hy hz] :pos [vx vy vz] :path vl :len}]
  (let [dx (- ex hx)
        dy (- ey hy)
        dz (- ez hz)
        el (vec-len [bx by bz])]
    (/ (+ (* bx (- (* dy vz) (* dz vy)))
          (* by (- (* dz vx) (* dx vz)))
          (* bz (- (* dx vy) (* dy vx))))
       el vl)))

(defn calc-derivs [[bx by bz] [ex ey ez] {[hx hy hz] :pos [vx vy vz] :path :as h}]
  (let [dx (- ex hx)
        dy (- ey hy)
        dz (- ez hz)
        o (calc-offset [bx by bz] [ex ey ez] h)
        deriv [[(- (* dy vz) (* dz vy))
                (- (* dz vx) (* dx vz))
                (- (* dx vy) (* dy vx))]
               [(- (* bz vy) (* by vz))
                (- (* bx vz) (* bz vx))
                (- (* by vx) (* bx vy))]]]
    (vec (map #(vec (map (partial * 2 o) %)) deriv))))

(defn calc-total-offset [b e hailstones]
  (apply + (for [h hailstones
                 :let [o (calc-offset b e h)]]
             (* o o))))

(defn calc-total-derivs [b e hailstones]
  (println b e)
  (let [dbsdes (for [h hailstones] (calc-derivs b e h))]
    (reduce #(vector (map + (first %1) (first %2)) (map + (second %1) (second %2))) dbsdes)))

(defn calc-delta [b e hailstones]
  (let [base (calc-total-offset b e hailstones)
        dx (calc-total-offset (map + b [1e-7 0 0]) e hailstones)
        dy (calc-total-offset (map + b [0 1e-7 0]) e hailstones)
        dz (calc-total-offset (map + b [0 0 1e-7]) e hailstones)
        di (calc-total-offset b (map + e [1e-7 0 0]) hailstones)
        dj (calc-total-offset b (map + e [0 1e-7 0]) hailstones)
        dk (calc-total-offset b (map + e [0 0 1e-7]) hailstones)]
    [(map (partial - base) [dx dy dz]) (map (partial - base) [di dj dk])]))

(defn find-intersect-line [r [xx yy zz ii jj kk] hailstones]
  (let [s (* r 5)
        spread #(range (- % s) (+ % s) r)
        results
        (for [x (spread xx)
              y (spread yy)
              z (spread zz)
              i (spread ii)
              j (spread jj)
              k (spread kk)
              :when (not (and (zero? i) (zero? j) (zero? k)))]
          (let [o (calc-total-offset [i j k] [x y z] hailstones)]
            [o x y z i j k]))
        best (apply min-key first results)]
    (println "Recur" r best)
    (if (= r 1)
      best
      (recur (int (math/ceil (/ r 2))) (rest best) (take 5 hailstones)))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [hailstones (process (line-seq rdr))]
      (println hailstones)
      (find-intersect-line 3e13 [3e14 3e14 3e14 0 0 0] hailstones)
      #_(printf "Sum is: %d\n" (check-all-collisions hailstones))
      ))
)
