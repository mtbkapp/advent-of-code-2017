(ns advent-of-code-2017.day8
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.java.io :as io]))


(spec/def ::instruction
  (spec/cat :reg symbol?
            :reg-op '#{dec inc}
            :reg-operand number?
            :if #(= 'if %)
            :cond-reg symbol?
            :cond-op '#{== != >= > <= <}
            :cond-operand number?))

(defn parse-line
  [line]
  (spec/conform ::instruction (read-string (str "[" line "]"))))

(deftest test-parse-line
  (is (= '{:reg t 
           :reg-op inc 
           :reg-operand 245 
           :if if 
           :cond-reg xq 
           :cond-op != 
           :cond-operand 0}
         (parse-line "t inc 245 if xq != 0"))))

(defn parse-input
  []
  (with-open [rdr (io/reader (io/resource "day8.txt"))]
    (mapv parse-line (line-seq rdr))))

(deftest can-parse-input
  (is (zero? (count (filter #(= :clojure.spec.alpha/invalid %) (parse-input))))))

(def cond-op-fn
  {'== =
   '!= not=
   '> > 
   '>= >= 
   '< < 
   '<= <=})

(defn run?
  [registers {:keys [cond-reg cond-op cond-operand]}]
  (if-let [op-fn (cond-op-fn cond-op)]
    (op-fn (get registers cond-reg 0) cond-operand)))

(def reg-op-fn
  {'inc +
   'dec -})

(defn interpret*
  [registers {:keys [reg reg-op reg-operand]}]
  (if-let [op-fn (reg-op-fn reg-op)]
    (update registers reg (fnil op-fn 0) reg-operand)
    registers))

(defn interpret
  [program]
  (reduce (fn [registers instruction]
            (if (run? registers instruction)
              (interpret* registers instruction)
              registers))
          {}
          program))

(defn max-register
  [registers]
  (reduce max (vals registers)))

(def example-program "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(defn read-example-input
  []
  (map parse-line (string/split-lines example-program)))

#_(max-register (interpret (read-example-input)))

#_(solve-part1)
(defn solve-part1
  []
  (max-register (interpret (parse-input))))

(defn max-value-during-exec
  [program]
  (first (reduce (fn [[max-val registers] {:keys [reg] :as instruction}]
                   (if (run? registers instruction)
                     (let [next-registers (interpret* registers instruction)]
                       [(max max-val (get next-registers reg 0)) next-registers])
                     [max-val registers]))
                 [0 {}]
                 program)))

#_(solve-part2)
(defn solve-part2
  []
  (max-value-during-exec (parse-input)))

