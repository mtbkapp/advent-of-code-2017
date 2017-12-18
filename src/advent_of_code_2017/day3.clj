(ns advent-of-code-2017.day3
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn dup
  [[x & xs]]
  (lazy-seq
    (when x
      (list* x x (dup xs)))))

(defn prepend-all
  [[h & heads] tail]
  (lazy-seq
    (if (some? h)
      (cons h (prepend-all heads tail))
      tail)))

#_(take 10 (drop 20000 (spiral)))
#_(take 20 (spiral))
(defn spiral
  ([] (spiral (cycle [:right :up :left :down])
                  (dup (rest (range)))))
  ([[d & dirs] [n & nums]]
   (lazy-seq
     (prepend-all (repeat n d) (spiral dirs nums)))))

(defmulti move (fn [coord dir] dir))

(defmethod move :up
  [[x y] _]
  [x (inc y)])

(defmethod move :down
  [[x y] _]
  [x (dec y)])

(defmethod move :right
  [[x y] _]
  [(inc x) y])

(defmethod move :left
  [[x y] _]
  [(dec x) y])

(defn address->coord
  [address]
  (reduce (fn [coord direction]
            (move coord direction))
          [0 0]
          (take (dec address) (spiral))))

#_(solve-part1 368078)
(defn solve-part1
  [address]
  (let [[x y] (address->coord address)]
    (+ (Math/abs x) (Math/abs y))))

(deftest solves-examples
  (is (= 0 (solve-part1 1)))
  (is (= 3 (solve-part1 12)))
  (is (= 2 (solve-part1 23)))
  (is (= 31 (solve-part1 1024))))

(defn adjacent-coords 
  [[x y]]
  (for [dx [1 0 -1] dy [1 0 -1]
        :when (not= [0 0] [dx dy])]
    [(+ x dx) (+ y dy)]))

(defn calc-sum 
  [space coord]
  (transduce (map #(get space % 0))
             +
             (adjacent-coords coord)))

#_(solve-part2 368078)
(defn solve-part2
  [address]
  (loop [space {[0 0] 1}
         coord [1 0]
         sum 1 
         [dir & ds] (rest (spiral))]
    (if (> sum address)
      sum
      (let [sum (calc-sum space coord)]
        (recur (assoc space coord sum) 
               (move coord dir)
               sum
               ds)))))

