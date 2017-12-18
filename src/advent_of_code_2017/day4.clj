(ns advent-of-code-2017.day4
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(defn unique-words?
  [words]
  (= (count words)  
     (count (distinct words))))

(defn solve
  [xform]
  (with-open [rdr (io/reader (io/resource "day4.txt"))]
    (transduce xform + (line-seq rdr))))

#_(solve-part1)
(defn solve-part1
  []
  (solve
    (comp (map #(string/split % #"\s+")) 
          (map unique-words?)
          (map #(if (true? %) 1 0))))) 

#_(solve-part2)
(defn solve-part2
  []
  (solve
    (comp (map #(string/split % #"\s+"))
          (map #(map (fn [w] (into #{} (seq w))) %))
          (map unique-words?)
          (map #(if (true? %) 1 0)))))

