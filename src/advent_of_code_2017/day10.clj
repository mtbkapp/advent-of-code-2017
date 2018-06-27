(ns advent-of-code-2017.day10
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))



(defn rope
  [size]
  (vec (range size)))

(defn twist
  [rope start len]
  (splice rope start
          (->> (cycle rope)
               (drop start)
               (take len)
               (reverse))))

(defn splice
  [rope start slice]
  (-> (reduce (fn [[i rope] n]
                [(inc i)
                 (assoc rope (mod (+ start i) (count rope)) n)])
              [0 rope]
              slice)
      second))

(defn knot-hash
  [size lengths]
  (nth
    (reduce (fn [[skip pos rope] len]
              [(inc skip) (+ pos len skip) (twist rope pos len)])
            [0 0 (rope size)]
            lengths)
    2))

(defn solve-part-1
  []
  (let [lens  [212 254 178 237 2 0 1 54 167 92 117 125 255 61 159 164]
        r (knot-hash 256 lens)]
    (* (first r) (second r))))


(deftest test-part-1
  (is (= [0 1 3 2 4 5 6 7 8 9]
         (twist (rope 10) 2 2)))
  (is (= [3 1 2 0 4] (twist (rope 5) 3 3)))
  (is (= [3 4 2 1 0] (twist [4 3 0 1 2] 1 5)))
  (is (= [3 4 2 1 0] (knot-hash 5 [3 4 1 5])))
  (is (= 212 (solve-part-1)))
  )

