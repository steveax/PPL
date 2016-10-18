(ns coins.core
  (:use clojure.set)
  (:require 
    (incanter [core :refer :all]
              [stats :refer :all])))

(defn roll-die [die-faces] (sample (range 1 (inc die-faces)) :size 1))

(defn roll-and-toss [d1 num-outcomes consec]
 (fn [roll-outcome] 
   (let [
        ways-of-winning (if (not= consec "c") (choose roll-outcome num-outcomes) 1)
        prob-coin-toss (/ 1 2)
        prob-outcomes-true (Math/pow prob-coin-toss num-outcomes)
        prob-rest-not-true (Math/pow prob-coin-toss (- roll-outcome num-outcomes))]
    
        (if (< roll-outcome num-outcomes)
          0
          (*
            ways-of-winning
            prob-outcomes-true
            prob-rest-not-true)))))


(defn generalized [d1 num-outcomes consec]
  (let [potential-outcomes (range 1 (inc d1))
        outcome-likelihood (roll-and-toss d1 num-outcomes consec)
        likelihood-list (map outcome-likelihood potential-outcomes)]
    likelihood-list))


(defn problem-f []
  (let [likelihoods (generalized 6 3 "nc")
        adjusted-likelihoods (map #(* (/ 1 6) %) likelihoods) ]
    (reduce + adjusted-likelihoods)))

(defn problem-g []
  (let [likelihoods (map #(generalized 6 % "nc") (range 3 7))
        adjustor (fn [likelihood-lists] (map #(* (/ 1 6) %) likelihood-lists))
        adjusted-likelihoods (map adjustor likelihoods)]
    (reduce + (map #(reduce + %) adjusted-likelihoods))))

(defn problem-h []
  (let [likelihoods (generalized 6 3 "c")
        adjusted-likelihoods (map #(* (/ 1 6) %) likelihoods) ]
    (reduce + adjusted-likelihoods)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
