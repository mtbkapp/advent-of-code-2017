(ns advent-of-code-2017.day5
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(defn read-input
  []
  (with-open [rdr (io/reader (io/resource "day5.txt"))]
    (mapv #(Integer/parseInt %) (line-seq rdr))))

#_(solve-part1 (read-input))
(defn solve-part1
  [program]
  (let [^ints program (int-array program)
        size (count program)]
    (loop [jump-count 0 i 0]
      (if (< i size)
        (let [offset (aget program i)
              next-index (+ i offset)]
          (aset program i (inc offset))
          (recur (inc jump-count) next-index))
        jump-count))))

#_(solve-part2 (read-input))
#_(solve-part2 [0 3 0 1 -3])
(defn solve-part2
  [program]
  (let [^ints program (int-array program)
        size (count program)]
    (loop [jump-count 0 i 0]
      (if (< i size)
        (let [offset (aget program i)]
          (if (>= offset 3)
            (aset program i (dec offset))
            (aset program i (inc offset)))
          (recur (inc jump-count) (+ i offset)))
        jump-count))))

; Notes
; Saving the size of the array in a local is critical to the speed of part2
; volatiles prevents some JVM optimizations because it forces the JVM to read
;  the value from memory everytime.

