(ns advent-of-code-2017.day10
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))


(defn make-rope
  [size]
  (vec (range size)))

(defn slice
  [rope pos len]
  (let [end (mod (+ pos len) (count rope))]
    (cond 
      (zero? len) []
      (< pos end) (subvec rope pos (+ pos len))
      :else  (into (subvec rope pos (count rope))
                   (subvec rope 0 end)))))

(deftest test-slice
  (is (= [0 1 2] (slice (make-rope 5) 0 3)))
  (is (= [3 4 0] (slice (make-rope 5) 3 3)))
  (is (= [3 4 2 1] (slice [2 1 0 3 4] 3 4)))
  (is (= [] (slice [0 1 2 3 4] 2 0)))
  (is (= [3 0 1 2 4] (slice [4 3 0 1 2] 1 5))))

(defn splice
  [sub-rope rope pos]
  (reduce (fn [rope [i x]]
            (assoc rope i x))
          rope
          (zipmap (map #(mod % (count rope))
                       (range pos (+ pos (count rope))))
                  sub-rope)))

(deftest test-splice
  (is (= [0 2 1 3 4] (splice [2 1] (make-rope 5) 1)))
  (is (= [4 3 2 1 0] (splice [1 0 4 3] (make-rope 5) 3)))
  (is (= [4 3 0 1 2] (splice [1 2 4 3] [2 1 0 3 4] 3))))

(defn update-rope
  [rope pos len]
  (-> (slice rope pos len)
      (reverse)
      (splice rope pos)))

(deftest test-update-rope
  (is (= [2 1 0 3 4] (update-rope (make-rope 5) 0 3)))
  (is (= [4 3 0 1 2] (update-rope [2 1 0 3 4] 3 4)))
  (let [rope (make-rope 10)]
    (is (= rope (update-rope rope 5 0)))))

(defn knot-hash-faster
  "Returns: [last-skip last-position last-rope-state]"
  ([rope [len & lengths] skip pos]
   (if len
     (recur (update-rope rope pos len)
            lengths
            (inc skip) 
            (mod (+ pos len skip) (count rope))) 
     [skip pos rope]))
  ([rope lengths]
   (knot-hash-faster rope lengths 0 0)))

(deftest test-knot-hash-faster
  (let [lengths [3 4 1 5]
        rope [0 1 2 3 4]
        [skip pos rope] (knot-hash-faster rope lengths 0 0)]
    (is (= [3 4 2 1 0] rope))
    (is (= 4 skip))
    (is (= 4 pos))))

(defn solve-part-1
  []
  (let [lens [212 254 178 237 2 0 1 54 167 92 117 125 255 61 159 164]
        r (nth (knot-hash-faster (make-rope 256) lens) 2)]
    (* (first r) (second r))))

(deftest test-part-1
  (is (= [3 4 2 1 0] (nth (knot-hash-faster (make-rope 5) [3 4 1 5]) 2)))
  (is (= 212 (solve-part-1))))

(defn xor-blocks
  [rope block-size]
  (map (fn [block]
         (reduce (fn [x y] (bit-xor x y)) block))
       (partition block-size rope)))

(deftest test-xor-blocks
  (is (= [0 5] (xor-blocks [2 2 1 4] 2))))

(defn to-hex
  [byte-seq]
  (str (reduce (fn [builder x]
                 (let [high (bit-shift-right (bit-and 2r11110000 x) 4)
                       low (bit-and 2r1111 x)
                       hex-digit #(nth ["0" "1" "2" "3"
                                        "4" "5" "6" "7"
                                        "8" "9" "a" "b"
                                        "c" "d" "e" "f"] %)]
                   (-> builder
                       (.append (hex-digit high))
                       (.append (hex-digit low)))))
               (StringBuilder.)
               byte-seq)))

(deftest test-to-hex
  (is (= "00" (to-hex [0])))
  (is (= "0a" (to-hex [10])))
  (is (= "ff09f0fabe" (to-hex [255 9 240 250 190]))))

(defn post-process
  [rope]
  (to-hex (xor-blocks rope 16)))

(def special-seq [17 31 73 47 23])

(defn input-from-string
  [s]
  (into (mapv int s) special-seq))

(defn full-knot-hash
  [input rope-size rounds]
  (loop [r rounds [skip pos rope] [0 0 (make-rope rope-size)]]
    (if (zero? r)
      (post-process rope)
      (recur (dec r) (knot-hash-faster rope input skip pos)))))

(deftest test-full-knot-hash
  (is (= 32 (count (full-knot-hash (input-from-string "") 256 64))))
  (is (= "a2582a3a0e66e6e86e3812dcb672a272"
         (full-knot-hash (input-from-string "") 256 64)))
  (is (= "33efeb34ea91902bb2f59c9920caa6cd"
         (full-knot-hash (input-from-string "AoC 2017") 256 64)))
  (is (= "3efbe78a8d82f29979031a4aa0b16a9d"
         (full-knot-hash (input-from-string "1,2,3") 256 64)))
  (is (= "63960835bcdc130f0b66d7ff4f6a5a8e"
         (full-knot-hash (input-from-string "1,2,4") 256 64))))

(defn solve-part2
  []
  (full-knot-hash (input-from-string "212,254,178,237,2,0,1,54,167,92,117,125,255,61,159,164") 
                  256 64))

(deftest test-solve-part2
  (is (= "96de9657665675b51cd03f0b3528ba26" (solve-part2))))

