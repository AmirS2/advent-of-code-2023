(ns aoc2023.day-7.part2
  (:require [clojure.string :as str]))

(def card-vals
  {\A 14
   \K 13
   \Q 12
   \J 0
   \T 10
   \9 9
   \8 8
   \7 7
   \6 6
   \5 5
   \4 4
   \3 3
   \2 2
   \1 1})

(defn parse-hand [line]
  (let [[cards_str bid_str] (str/split line #" ")
        bid (Integer/parseInt bid_str)
        cards (seq cards_str)]
    {:cards (map card-vals cards)
     :bid bid}))

(defn parse-lines [lines]
  (map parse-hand lines))

(defn get-full-type [freq-vals]
  (case freq-vals
    [5] 8         ; five of a kind
    [1 4] 7       ; four of a kind
    [2 3] 6       ; full house
    [1 1 3] 5     ; three of a kind
    [1 2 2] 4     ; two pair
    [1 1 1 2] 3   ; pair
    [1 1 1 1 1] 2 ; high card
    ))

(defn get-type [hand]
  (let [freqs (frequencies hand)
        joker-count (freqs 0)
        freqs-regular (frequencies (filter (comp not zero?) hand))
        freq-vals (sort (vals freqs-regular))]
    (case joker-count
      nil (get-full-type freq-vals)
      1 (case freq-vals
          [4]  8       ; becomes 5
          [1 3] 7      ; becomes 4
          [2 2] 6      ; becomed full house
          [1 1 2] 5    ; becomes 3 of a kind
          [1 1 1 1] 3) ; becomes a pair
      2 (case freq-vals
          [3] 8        ; becomes 5
          [1 2] 7      ; becomes 4 of a kind
          [1 1 1] 5)   ; becomes 3 of a kind
      3 (case freq-vals
          [2] 8        ; becomes 5
          [1 1] 7)     ; becomes 4 of a kind
      4 8              ; becomes 5 of a kind
      5 8              ; 5 of a kind
      )))

(defn compare-hands [{hand1 :cards} {hand2 :cards}]
  (let [type1 (get-type hand1)
        type2 (get-type hand2)]
    (if (= type1 type2)
      ; if equal hand types, ompare by card val in order
      (first (filter (comp not zero?) (map compare hand1 hand2)))
      (compare type1 type2))))

(defn score-hand [idx hand]
  (* (+ idx 1) (:bid hand)))

(defn -main
  "Read the input and solve it"
  [& args]
  (with-open [rdr (clojure.java.io/reader *in*)]
    (let [hands (parse-lines (line-seq rdr))
          sorted-hands (sort compare-hands hands)
          winnings (apply + (map-indexed score-hand sorted-hands))]
      (dorun (map println sorted-hands))
      (printf "Winnings are: %d\n" winnings)))
)
