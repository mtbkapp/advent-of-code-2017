(ns advent-of-code-2017.day13
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io Reader]))


(def test-firewall
  "0: 3
1: 2
4: 4
6: 4")

(defn parse-firewall
  [firewall]
  (into {}
        (map (comp (juxt (comp read-string first)
                         (comp read-string second)) 
                   #(string/split % #"\s*:\s*"))) 
        (string/split-lines firewall)))

(deftest test-parse-firewall 
  (is (= {0 3, 1 2, 4 4, 6 4}
         (parse-firewall test-firewall))))

(defn add-initial-scanner-state
  [{:keys [firewall] :as state}]
  (assoc state :scanners (zipmap (keys firewall)
                                 (repeat [1 0]))))

(deftest test-add-initial-scanner-state
  (let [firewall (parse-firewall test-firewall)]
    (is (= {:firewall firewall :scanners {0 [1 0], 
                                          1 [1 0], 
                                          4 [1 0], 
                                          6 [1 0]}}
           (add-initial-scanner-state {:firewall firewall})))))

(defn next-scanner
  [firewall depth [dir pos]]
  (let [next-pos (+ pos dir)
        depth-range (dec (get firewall depth))]
    (cond (= next-pos 0) [1 0] 
          (= next-pos depth-range) [-1 depth-range]
          :else [dir next-pos])))

(deftest test-next-scanner
  (let [firewall {0 4}
        depth 0
        states (iterate (partial next-scanner firewall depth) [1 0])]
    (is (= [1 0] (first states)))
    (is (= [1 1] (second states)))
    (is (= [1 2] (nth states 2)))
    (is (= [-1 3] (nth states 3)))
    (is (= [-1 2] (nth states 4)))
    (is (= [-1 1] (nth states 5)))
    (is (= [1 0] (nth states 6)))
    (is (= [1 1] (nth states 7))))
  (is (= [[1 0] [-1 1] [1 0] [-1 1]] 
         (take 4 (iterate (partial next-scanner {0 2} 0) [1 0])))))

(defn update-scanner-positions
  [{:keys [scanners firewall] :as state}]
  (assoc state :scanners 
         (into {} (map (fn [[depth current]]
                         [depth (next-scanner firewall depth current)]))
               scanners)))

(deftest test-update-scanner-positions
  (is (= {:scanners {0 [1 1] 1 [1 1]}
          :firewall {0 4, 1 5} }
         (update-scanner-positions 
           {:scanners {0 [1 0] 1 [1 0]}
            :firewall {0 4, 1 5} }))))

(defn move-packet
  [state]
  (update state :packet inc))

(defn next-state
  [current]
  (-> (move-packet current)
      (update-scanner-positions)))

(defn max-layer
  [firewall]
  (reduce max (keys firewall)))

(defn render-state
  [{:keys [firewall scanners packet] :as state}]
  (let [layers (range (inc (max-layer firewall)))
        max-range (reduce max (vals firewall))]
    (doseq [s (interpose " " (map (fn [x] (str " " x " ")) layers))]
      (print s))
    (println)
    (doseq [row (range max-range)]
      (doseq [col layers]
        (let [packet? (and (= row 0) (= packet col))]
          (if (< row (get firewall col -1))
            (do
              (print (if packet? \( \[))
              (print (if (= row (second (get scanners col))) \S \space))
              (print (if packet? \) \]))
              (print \space))
            (if packet? 
              (print "( ) ")
              (print "    ")))))
      (println))))

(defn initial-state
  [firewall]
  (-> {:firewall firewall}
      (add-initial-scanner-state)
      (assoc :packet -1)))

#_(doseq [[s1 s2] (gen-states (initial-state (parse-firewall test-firewall)))]
  (println "..........................................................")
  (render-state s1)
  (render-state s2)
  (println ".........................................................."))
(defn state-seq
  ([state]
   (state-seq state true))
  ([state move-packet?]
   (lazy-seq
     (cons state (state-seq ((if move-packet? 
                               move-packet 
                               update-scanner-positions)
                             state) 
                            (not move-packet?))))))

(defn gen-states
  [{:keys [firewall] :as state}]
  (let [last-layer (max-layer firewall)
        packet-in? #(<= (:packet %) last-layer)]
    (->> (state-seq state)
         (rest) 
         (take-while packet-in?)
         (partition 2))))

(defn caught?
  [[{:keys [firewall scanners packet] :as state} _]]
  (= 0 (get-in scanners [packet 1])))

(deftest test-caught?
  (let [[a b c d e f g] (gen-states (initial-state (parse-firewall test-firewall)))]
    (is (caught? a))
    (is (not (caught? b)))
    (is (not (caught? c)))
    (is (not (caught? d)))
    (is (not (caught? e)))
    (is (not (caught? f)))
    (is (caught? g))))

(defn severity
  [firewall]
  (reduce (fn [severity [{:keys [packet]} _ :as pico-states]]
            (if (caught? pico-states)
              (+ severity (* packet (get firewall packet)))
              severity))
          0 
          (gen-states (initial-state firewall))))

(deftest test-severity
  (is (= 24 (severity (parse-firewall test-firewall))))
  (is (= 2160 (severity (parse-firewall (slurp (io/resource "day13.txt")))))))

(defn delay-packet
  [state duration]
  (first (drop duration (iterate update-scanner-positions state))))

(defn delay-seq
  [state]
  (->> (iterate update-scanner-positions state)
       (interleave (range))
       (partition 2)))

(defn find-safe-delay
  [firewall]
  (first
    (sequence
      (comp (map (fn [[wait initial-state]]
                   [wait (some caught? (gen-states initial-state))]))
            (remove second)
            (map first))
      (delay-seq (initial-state firewall)))))

; solution is slow, answer = 3907470
#_(find-safe-delay (parse-firewall (slurp (io/resource "day13.txt"))))






