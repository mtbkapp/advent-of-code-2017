(ns advent-of-code-2017.day6
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]))


(def input [2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14])

(defn find-max
  [banks]
  (reduce (fn [[mi mx :as mv] [i x :as v]]
            (if (> x mx) v mv))
          [Long/MAX_VALUE Long/MIN_VALUE]
          (map-indexed vector banks)))

(defn next-idx
  [idx size]
  (mod (inc idx) size))

(defn redist
  ([banks]
   (let [[max-idx max-x] (find-max banks)]
     (redist (assoc banks max-idx 0)
             max-x
             (next-idx max-idx (count banks)))))
  ([banks blocks idx]
   (if (> blocks 0)
     (recur (assoc banks idx (inc (nth banks idx)))
            (dec blocks)
            (next-idx idx (count banks)))
     banks)))

#_(take 12 (iterate redist [0 2 7 0]))
#_(solve-part1 [0 2 7 0])
#_(solve-part1 input)
(defn solve-part1
  [start]
  (loop [[b & bs] (iterate redist start) seen #{}]
    (if (contains? seen b)
      (count seen)
      (recur bs (conj seen b)))))


#_(solve-part2 [0 2 7 0])
#_(solve-part2 input)
(defn solve-part2
  [start]
  (loop [[b & bs] (iterate redist start) seen {} i 0]
    (if (contains? seen b)
      (- i (get seen b))
      (recur bs (assoc seen b i) (inc i)))))

