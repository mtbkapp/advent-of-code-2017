(ns advent-of-code-2017.day1
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]))


(defn solve-part1
  [[x :as input]]
  (transduce (comp (map (fn [[x y]]
                          (if (= x y)
                            x)))
                   (remove nil?))
             +
             (partition 2 1 (into input [x]))))

(deftest test-examples
  (is (= 3 (solve-part1 [1 1 2 2])))
  (is (= 4 (solve-part1 [1 1 1 1])))
  (is (= 0 (solve-part1 [1 2 3 4])))
  (is (= 9 (solve-part1 [9 1 2 1 2 1 2 9]))))

(defn parse-string-input
  [string]
  (butlast (map #(Character/getNumericValue %) string)))

(deftest test-parse-string-input
  (is (= (range 10) (parse-string-input (string/join (range 10))))))


(comment
  (solve-part1 (parse-string-input (slurp (io/resource "day1.txt"))))
  )


(defn solve-part2
  [input]
  (let [cnt (count input)
        half (/ cnt 2)]
    (reduce (fn [sum n]
              (if (= (nth input n)
                     (nth input (mod (+ n half) cnt)))
                (+ sum (nth input n))
                sum))
            0
            (range cnt))))

(deftest test-part2-examples
  (is (= 6 (solve-part2 [1 2 1 2])))
  (is (= 0 (solve-part2 [1 2 2 1])))
  (is (= 4 (solve-part2 [1 2 3 4 2 5])))
  (is (= 12 (solve-part2 [1 2 3 1 2 3])))
  (is (= 4 (solve-part2 [1 2 1 3 1 4 1 5]))))

(comment 
  (solve-part2 (parse-string-input (slurp (io/resource "day1_part2.txt"))))
  )
