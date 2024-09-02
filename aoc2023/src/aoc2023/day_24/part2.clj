(ns aoc2023.day-24.part2
  (:require [clojure.string :as str]
            [clojure.math :as math]))

(defn vec-len [v]
  (math/sqrt (apply + (map #(* % %) v ))))

(defn parse-hailstone [line]
  (let [[px py pz vx vy vz] (map #(Long/parseLong %) (str/split line #" ?[,@] +"))]
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
        bl (vec-len [bx by bz])]
    (/ (+ (* bx (- (* dy vz) (* dz vy)))
          (* by (- (* dz vx) (* dx vz)))
          (* bz (- (* dx vy) (* dy vx))))
       bl vl)))

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
  (let [del 1e-1
        base (calc-total-offset b e hailstones)
        dx (calc-total-offset (map + b [del 0 0]) e hailstones)
        dy (calc-total-offset (map + b [0 del 0]) e hailstones)
        dz (calc-total-offset (map + b [0 0 del]) e hailstones)
        di (calc-total-offset b (map + e [del 0 0]) hailstones)
        dj (calc-total-offset b (map + e [0 del 0]) hailstones)
        dk (calc-total-offset b (map + e [0 0 del]) hailstones)
        calc #(/ (- % base) del)]
    [(map calc [dx dy dz]) (map calc [di dj dk])]))

(defn find-intersect-line [b e hailstones]
  (let [o (calc-total-offset b e hailstones)]
    (if (zero? o)
      [b e]
      (let [[db de] (calc-delta b e hailstones)
            ssq (apply + (map #(* % %) (concat db de)))
            s (math/sqrt ssq)
            delta (/ o s)
            bb (map - b (map #(/ (* delta %) s) db))
            ee (map - e (map #(/ (* delta %) s) de))]
        (println "Recur" bb ee o s delta db de)
        (recur bb ee hailstones)))))

(defn prime-factors [n factors]
  (if (= n 1)
    factors
    (let [next-fac (loop [t 2] (if (not= (mod n t) 0) (recur (inc t)) t))]
      (recur (/ n next-fac) (conj factors next-fac)))))

(defn gcd [a b]
  (last (for [x (range 1 (inc (abs a))) :when (= (mod a x) (mod b x) 0)] x)))

(defn find-possible [p1 p2 v1 v2 vels]
  (let [d (abs (- p2 p1))]
    (for [v vels
          :let [_ (println v p1 p2 v1 v2)]
          :when (and (not= v v2) (not= v2 v1))
          :when (= (mod d (gcd (abs (- v v2)) (abs (- v2 v1)))) 0)]
      v)))

(defn find-possible-velocities
  [[{[h1x h1y h1z] :pos [h1i h1j h1k] :path :as h1}
    {[h2x h2y h2z] :pos [h2i h2j h2k] :path :as h2}
    & remaining]
   vels]
  (println "FPV" h1x h2x h1i h2i vels)
  (if (empty? remaining)
    vels
    (recur (cons h1 remaining) (find-possible h1x h2x h1i h2i vels))))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [hailstones (process (line-seq rdr))
          possible-vels (find-possible-velocities hailstones (range 400))]
      (println hailstones)
      (println possible-vels)
      #_(printf "Sum is: %d\n" (check-all-collisions hailstones))
      ))
)
