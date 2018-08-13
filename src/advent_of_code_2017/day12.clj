(ns advent-of-code-2017.day12
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [instaparse.core :as insta])
  (:import [clojure.lang PersistentQueue]))


(def parser (insta/parser (io/resource "day12_input_grammar.g")))

#_(clojure.pprint/pprint (parser test-input))
(def test-input
"0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(defn build-graph
  [parsed-input]
  (reduce (fn [graph [_ node _ & adj-nodes]]
            (assoc graph node adj-nodes))
          {}
          parsed-input))

(deftest test-build-graph
  (is (= {"0" ["2"]
          "1" ["1"]
          "2" ["0" "3" "4"]
          "3" ["2" "4"]
          "4" ["2" "3" "6"]
          "5" ["6"]
          "6" ["4" "5"]})
      (build-graph (parser test-input))))

(defn connected-nodes
  [graph node]
  ; BFS
  (loop [q (conj (PersistentQueue/EMPTY) node) 
         visited #{}]
    (let [n (peek q)
          qs (pop q)]
      (if (empty? q)
        visited
        (if (contains? visited n)
          (recur qs visited)
          (recur (into qs (get graph n))
                 (conj visited n)))))))

(deftest test-connected-nodes
  (is (= #{"0" "2" "3" "4" "5" "6"} 
         (connected-nodes (build-graph (parser test-input)) "0"))))

#_(solve-part1)
(defn solve-part1
  []
  (-> (slurp (io/resource "day12.txt"))
      parser
      build-graph
      (connected-nodes "0")
      count))

(defn components
  ([graph] (components graph #{}))
  ([graph comps]
   (if (empty? graph)
     comps
     (let [n (key (first graph))
           nodes (connected-nodes graph n)]
       (recur (apply dissoc graph nodes)
              (conj comps nodes))))))

(deftest test-components
  (is (= #{#{"1"} #{"0" "2" "3" "4" "5" "6"}}
         (components (build-graph (parser test-input))))))

#_(solve-part2)
(defn solve-part2
  []
  (-> (slurp (io/resource "day12.txt"))
      parser
      build-graph
      components
      count))

