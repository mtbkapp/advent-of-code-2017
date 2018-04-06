(ns advent-of-code-2017.day8
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]))


(defn read-char-seq
  []
  (seq (slurp (io/resource "day9.txt"))))

#_(count (read-char-seq))
#_(prn (solve (read-char-seq)))
#_(solve (seq "{{<!>},{<!>},{<!>},{<a>}}"))
#_(prn (solve (seq "{{{},{},{{}}}}")))
#_(solve (seq "{{{}}}"))
#_(solve (seq "{{},{}}"))
#_(solve (seq "{<{o\"i!a,<{i<a>}"))

(defn solve
  [cs]
  (reduce (fn [{:keys [in-garbage? container-score ignore-next?] :as context} c]
            (if ignore-next?
              (assoc context :ignore-next? false)
              (condp = c
                \! (assoc context :ignore-next? true)
                \{  (if (not in-garbage?) 
                      (update context :container-score inc)
                      (update context :garbage-count inc))
                \}  (if (not in-garbage?) 
                      (-> context
                          (update :group-count inc)
                          (update :container-score dec)
                          (update :sum + (inc container-score)))
                      (update context :garbage-count inc))
                \< (if in-garbage?
                     (update context :garbage-count inc)
                     (assoc context :in-garbage? true))
                \> (assoc context :in-garbage? false)
                (if in-garbage?
                  (update context :garbage-count inc)
                  context))))
          {:in-garbage? false 
           :ignore-next? false
           :group-count 0
           :container-score -1 
           :sum 0
           :garbage-count 0}
          cs))
