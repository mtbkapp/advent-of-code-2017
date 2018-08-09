(ns advent-of-code-2017.day11
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io])
  (:import [java.io Reader]))


(def start [0 0 0])

(defmulti move (fn [coord dir] dir))

(defmethod move :hex/north
  [[x y z] _]
  [x (inc y) (dec z)])

(defmethod move :hex/north-east
  [[x y z] _]
  [(inc x) y (dec z)])

(defmethod move :hex/north-west
  [[x y z] _]
  [(dec x) (inc y) z])

(defmethod move :hex/south
  [[x y z] _]
  [x (dec y) (inc z)])

(defmethod move :hex/south-east
  [[x y z] _]
  [(dec x) y (inc z)])

(defmethod move :hex/south-west
  [[x y z] _]
  [(inc x) (dec y) z])

; Hex layout
;  \ n  /
;nw +--+ ne
;  /    \
;-+      +-
;  \    /
;sw +--+ se
;  / s  \

;coord = [north <-> south, north-east <-> south-west, north-west <-> south-east]
;taken from https://www.redblobgames.com/grids/hexagons/

(deftest test-move
  (is (= [0 1 -1] (move [0 0 0] :hex/north)))
  (is (= [1 0 -1] (move [0 0 0] :hex/north-east)))
  (is (= [-1 1 0] (move [0 0 0] :hex/north-west)))
  (is (= [0 -1 1] (move [0 0 0] :hex/south)))
  (is (= [-1 0 1] (move [0 0 0] :hex/south-east)))
  (is (= [1 -1 0] (move [0 0 0] :hex/south-west))))

(defn distance 
  [[ax ay az] [bx by bz]]
  (/ (+ (Math/abs (- bx ax))
        (Math/abs (- by ay))
        (Math/abs (- bz az))) 
     2))

(deftest test-distance
  (is (= 0 (distance start start)))
  (is (= 1 (distance start (move start :hex/north))))
  (is (= 1 (distance start (move start :hex/north-east))))
  (is (= 1 (distance start (move start :hex/north-west))))
  (is (= 1 (distance start (move start :hex/south))))
  (is (= 1 (distance start (move start :hex/south-east))))
  (is (= 1 (distance start (move start :hex/south-west)))))

(def dir-map
  {"n" :hex/north
   "ne" :hex/north-east
   "nw" :hex/north-west
   "s" :hex/south
   "se" :hex/south-east
   "sw" :hex/south-west})

(defn- lazy-read
  [^Reader rdr]
  (lazy-seq
    (let [c1 (.read rdr)
          c2 (.read rdr)]
      (if (= c1 -1)
        (do (.close rdr)
            ())
        (cons
          (get dir-map
               (if (= (char c2) \,)
                 (str (char c1))
                 (let [c3 (char (.read rdr))]
                   (if (and (not= c3 \,) (not= \n))
                     (throw (IllegalStateException. (str "Expected a comma or newline but got [" (char c3) "]"))))
                   (str (char c1) (char c2)))))
          (lazy-read rdr))))))

; state machine
; start c1 = nil, c2 = nil
; read 2 char
; case 1
;   c1 = direction
;   c2 = \,
; case 2
;   c1 = part of direction
;   c2 = 2nd part of direction
;   must read another char, which should be \, repeat until char = -1

(defn read-input
  []
  (with-open [rdr (io/reader (io/resource "day11.txt"))]
    (doall (lazy-read rdr))))

(distance 
  (reduce
    move
    start
    (read-input)))
