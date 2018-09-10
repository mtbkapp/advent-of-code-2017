(ns advent-of-code-2017.day14
  (:require [clojure.test :refer :all]
            [clojure.set :as sets]
            [advent-of-code-2017.day10 :as day10]
            [advent-of-code-2017.day12 :as day12]))


(def part-1-input "hwlqcszp")

(def hex->bin
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \a "1010"
   \b "1011"
   \c "1100"
   \d "1101"
   \e "1110"
   \f "1111"})

(defn hex-to-bin-str
  [hex]
  (str (reduce (fn [sb c]
                 (.append sb (hex->bin c)))
               (StringBuffer.)
               hex)))

(defn count-ones
  [bin]
  (reduce (fn [cnt chr]
            (if (= chr \1)
              (inc cnt)
              cnt)) 
          0
          bin))

(deftest test-count-ones
  (is (= 5 (count-ones (hex-to-bin-str "1f")))))

(defn process 
  [input]
  (comp hex-to-bin-str 
        #(day10/full-knot-hash (day10/input-from-string %) 256 64)
        #(str input "-" %)))

(defn solve-part1
  [input]
  (reduce + (map (comp count-ones (process input)) (range 0 128))))

(deftest test-solve-part1
  (is (= 8108 (solve-part1 "flqrgnkx")))
  (is (= 8304 (solve-part1 part-1-input))))


(defn build-matrix
  [input] 
  (mapv (comp vec (process input)) (range 0 128)))

(def matrix (build-adj-matrix part-1-input))

(defn matrix-get 
  [matrix [x y]]
  (get-in matrix [y x]))

(defn find-verticies
  [m]
  (into #{} (remove nil?)
        (for [x (range 128) 
              y (range 128)]
          (let [p [x y]]
            (if (= \1 (matrix-get m p)) p)))))

(def verticies (find-verticies matrix))

(defn neighbors
  [verticies [x y :as p]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [nx (+ x dx)
              ny (+ y dy)
              np [nx ny]
              d [dx dy]]
        :when (and (or (zero? dx)  ; no diagonals
                       (zero? dy)) 
                   (contains? verticies np))]
    np))


(defn build-adj-list
  [verticies]
  (reduce (fn [g [x y :as p]]
            (assoc g p (set (neighbors verticies p))))
          {}
          verticies))

; solution to part 2, 1018
#_(count (connected-components (build-adj-list verticies)))
(defn connected-components
  [graph]
  (loop [vs (set (keys graph)) components #{}]
    (if (empty? vs) 
      components
      (let [region (day12/connected-nodes graph (first vs))]
        (recur (sets/difference vs region)
               (conj components region))))))

