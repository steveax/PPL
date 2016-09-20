(ns die.core
  (:require [clojure.math.combinatorics :as combo]))


(defn possible-combos [d1-list d2-list]
  "Given two lists, create their"
  "the cartesian product admittedly can fuck the state space"
  (let [all-turns (combo/cartesian-product d1-list d2-list)]
    (map #(list
            (+ (first (first %)) (first (second %)))
            (* (second (first %)) (second (second %))))
         all-turns)))


(defn die-sums [d1 d2 number]
  "Given a number of faces of die 1 and die 2 and a number
  finds the probability of rolling that number as the sum of
  the two die"
  (let [ d1-list (map #(list %  (/ 1 d1)) (range 1 (inc d1)))
        d2-list (map #(list % (/ 1 d2)) (range 1 (inc d2)))
        all-possible  (possible-combos d1-list d2-list)] 
    (/ 
      (count (filter #(= number (first %)) all-possible))
      (count all-possible))))





(defn -main []
  (println "Run me in the REPL"))

