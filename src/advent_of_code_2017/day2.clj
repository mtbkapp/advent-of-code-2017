(ns advent-of-code-2017.day2
  (:require [clojure.java.io :as io]))

(defn parse-line
  [line]
  (map (comp #(Long/valueOf %) first) 
       (re-seq #"(\d+)" line)))

(defn row-min-max
  [[x & row-vals]]
  (reduce (fn [[low high :as acc] x]
            (cond (< x low) [x high]
                  (< high x) [low x]
                  :else acc))
          [x x]
          row-vals))

(defn solve
  [xform]
  (with-open [rdr (io/reader (io/resource "day2.txt"))]
    (transduce xform + (line-seq rdr))))

(defn solve-part1
  []
  (solve (comp (map parse-line)
               (map row-min-max)
               (map (fn [[low high]] (- high low))))))

(comment
  (solve-part1)
  )

(defn part2-row-result 
  [row]
  (for [x row y row :while (not= x y)]
    (cond (zero? (mod x y)) (/ x y)
          (zero? (mod y x)) (/ y x))))


(defn solve-part2
  []
  (solve (comp (map parse-line)
               (map part2-row-result)
               (map #(remove nil? %))
               (map first))))

(comment
  (solve-part2)
  )
