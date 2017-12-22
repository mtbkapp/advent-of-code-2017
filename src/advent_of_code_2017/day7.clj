(ns advent-of-code-2017.day7
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as spec]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import [clojure.lang ExceptionInfo]))

(spec/def ::name (spec/and symbol? #(re-matches #"\w+" (name %))))
(spec/def ::weight-list (spec/spec (spec/cat :weight int?)))
(spec/def ::arrow #(= '-> %))
(spec/def ::children (spec/cat :arrow ::arrow
                               :child-names (spec/+ ::name)))

(spec/def ::node (spec/cat :name ::name 
                           :weight-list ::weight-list
                           :children (spec/? ::children)))

(defn parse-line
  [line]
  (let [line' (str "[" line "]")
        {node-name :name
         {weight :weight} :weight-list
         {children :child-names} :children
         :as conformed} (spec/conform ::node (read-string line'))]
    (if (not= conformed :clojure.spec.alpha/invalid)
      {:name node-name
       :weight weight
       :children children}
      (throw (ex-info (str "Can't parse line: " line')
                      (spec/explain-data ::node line'))))))

(defn read-input
  []
  (with-open [rdr (io/reader (io/resource "day7.txt"))]
    (mapv parse-line (line-seq rdr))))

(defn build-parent-mapping
  [input]
  (reduce (fn [tree {:keys [name children]}]
            (if (some? children)
              (apply assoc tree (mapcat (juxt identity (constantly name)) children))
              tree))
          {}
          input))

(def example "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

(defn example-input
  []
  (map parse-line (string/split-lines example)))
(example-input)

(defn find-root
  ([tree] (find-root tree (key (first tree))))
  ([tree n]
   (if (contains? tree n)
     (recur tree (get tree n))
     n)))

(deftest test-find-root
  (let [tree (build-parent-mapping (map parse-line (string/split-lines example)))]
    (is (= 'tknk (find-root tree)))))


#_(prn (solve-part1))
(defn solve-part1
  []
  (find-root (build-parent-mapping (read-input))))

#_(clojure.pprint/pprint (build-tree (example-input)))
(defn build-tree
  [input]
  (reduce (fn [tree {:keys [name] :as node}]
            (assoc tree name node))
          {}
          input))

; exceptions as control flow! there is a better way, but this works.
; also the recursion could blow the stack, tail recursion or loop? 
#_(throw-on-unbalanced (build-tree (example-input)) 'tknk)
(defn throw-on-unbalanced
  [tree node-name]
  (let [{:keys [weight children]} (get tree node-name)
        child-burdens (map #(throw-on-unbalanced tree %) children)]
    (cond (empty? children) weight 
          (apply = child-burdens) (+ weight (apply + child-burdens))
          :else (throw (ex-info "Unbalanced!" (zipmap children child-burdens))))))

#_(prn (find-unbalanced (build-tree (example-input)) 'tknk))
(defn find-unbalanced
  [tree root]
  (try
    (throw-on-unbalanced tree root)
    (catch ExceptionInfo ex
      (ex-data ex))))

(defn calc-correction
  [row]
  (let [[a b :as vs] (sort (vals row))
        [z y] (reverse vs)]
    (cond (not= a b) (- b a)
          (not= z y) (- y z))))

(defn offender
  [row]
  (->> row
       (reduce (fn [acc [node-name weight]]
                 (update acc weight conj node-name))
               {})
       (some (fn [[weight nodes]]
               (if (= 1 (count nodes))
                 (first nodes))))))

#_(solve-part2 (example-input) 'tknk)
#_(solve-part2 (read-input) 'veboyvy)
(defn solve-part2
  [input root]
  (let [tree (build-tree input)
        row (find-unbalanced tree root)
        {bad-weight :weight} (get tree (offender row))]
    (+ bad-weight (calc-correction row))))

