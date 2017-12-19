(ns advent-of-code-2017.day5
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn read-input
  []
  (with-open [rdr (io/reader (io/resource "day5.txt"))]
    (long-array (map #(Long/valueOf %) (line-seq rdr)))))

#_(solve-part1 (read-input))
(defn solve-part1
  [input]
  (loop [jump-count 0 i 0]
    (if (< i (count input))
      (let [offset (aget input i)
            next-index (+ i offset)]
        (aset input i (inc offset))
        (recur (inc jump-count) next-index))
      jump-count)))

(def go (atom false))

; TOO SLOW!
#_(solve-part2 (read-input))
(defn solve-part2
  [input]
  (reset! go true)
  (loop [jump-count 0 i 0]
    (spit "log" i :append true)
    (if (and (< i (count input))
             @go)
      (let [offset (aget input i)
            next-index (+ i offset)]
        (aset input i (if (<= 3 offset) 
                        (dec offset)
                        (inc offset)))
        (recur (inc jump-count) next-index))
      jump-count)))

; TOO SLOW!
(set! *warn-on-reflection* true)
(defn solve-part2-faster
  [program]
  (let [size 1097
        jumpCount (volatile! 0)
        i (volatile! 0)]
    (while (< @i size)
      (if (zero? (mod @jumpCount 10000)) (prn @jumpCount))
      (let [^long offset (aget program ^long @i)]
        (if (>= offset 3)
          (aset program ^long @i (- offset 1))
          (aset program ^long @i (+ offset 1)))
        (vswap! i #(+ % offset))
        (vswap! jumpCount inc)))
    @jumpCount))

(deftest test-part2-example
  (let [arr (long-array [0 3 0 1 -3])]
    (is (= 10 (solve-part2 arr)))
    (prn (seq arr))))


