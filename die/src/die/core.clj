(ns die.core
  (:require 
    [clojure.math.combinatorics :as combo]))


(defn possible-combos [d1-list d2-list]
  "Given two lists, create a list of tuples possible turns
  in the form (sum-die likelihood) the cartesian product is admittedly
  terrible for the state space
  (many possibilities need to be represented without being inferred)
  but works nicely for real-world-sided die"
  (let [all-turns (combo/cartesian-product d1-list d2-list)]
    (map #(list
            (+ (first (first %)) (first (second %)))
            (* (second (first %)) (second (second %))))
         all-turns)))


(defn die-sums [d1 d2 number operator]
  "Given a number of faces of die 1 and die 2 and a number
  finds the probability of rolling that number as the operation of
  the two die"
  
  "Operation is a binary numeral operator like = < >"
  
  (let [ d1-list (map #(list %  (/ 1 d1)) (range 1 (inc d1)))
        d2-list (map #(list % (/ 1 d2)) (range 1 (inc d2)))
        all-possible  (possible-combos d1-list d2-list)] 
    (/ 
      (count (filter #(operator number (first %)) all-possible))
      (count all-possible))))




(defn choose [n k]
    (if (or (= n k) (= k 0))
        1
        (if (< n k)
            0
            (+ 
                (choose (dec n) k)
                (choose (dec n) (dec k))))))


(defn problem-n [d1 d2 pivot sheet-placement total-turns num-in]
  "Roll a d1 faced and d2 faced die"
  
  "Choose a pivot number and make 'left' center' and 'right' columns on a sheet"
  
  "If the sum of the die faces is less than the pivot, put a tick in the left
column, if it's equal put it in the center, and greater than in the right"

"What is the probability of getting exactly num-in
ticks in column sheet-placement = "left" | "center" | "right" after total-turns"
 
  (let 
    [ prob-in (cond
                 (= sheet-placement "left")
                 (die-sums d1 d2 pivot <)
                 (= sheet-placement "center")
                 (die-sums d1 d2 pivot =)
                 (= sheet-placement "right")
                 (die-sums d1 d2 pivot >))
     
     prob-out (- 1 prob-in)
     num-out  (- total-turns num-in)
     ways-of-winning (choose total-turns num-in)
     prob-n-in (Math/pow prob-in num-in)
     prob-rest-out (Math/pow prob-out num-out)
     
     ]    
   (*
     ways-of-winning
     prob-n-in
     prob-rest-out)))
     
;; Assume we have just d6 d8 d12 and d20
;;                    Usage
;; (key (apply max-key val (die-predict sum-of-die-faces)))
(defn die-predict [sum-num]
  (let [
        possibility-map
        {
         :d6-d6 (die-sums 6 6 sum-num =)
         :d6-d8 (die-sums 6 8 sum-num =)
         :d6-d12 (die-sums 6 12 sum-num =)
         :d6-d20 (die-sums 5 20 sum-num =)
         
         :d8-d8 (die-sums 8 8 sum-num =)
         :d8-d12 (die-sums 8 12 sum-num =)
         :d8-d20 (die-sums 8 20 sum-num =)
         
         
         :d12-d20 (die-sums 12 20 sum-num =)
         :d12-d12 (die-sums 12 12 sum-num =)
         
         :d20-d20 (die-sums 20 20 sum-num =)
         }
        ]
    
    possibility-map ))




(defn -main []
  (println "Load in the REPL"))

