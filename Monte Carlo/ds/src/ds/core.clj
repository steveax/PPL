(ns ds.core
  (:use clojure.set)
  (:require 
    (incanter [core :refer :all]
              [stats :refer :all]
              [charts :refer :all])))


(defn cartesian-product
  "All the ways to take one item from each sequence From clojure contrib library"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                      (if-let [rst (next (v-seqs i))]
                        (assoc v-seqs i rst)
                        (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
              (cons (map first v-seqs)
                    (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn possible-combos [d1-list d2-list]
  "Given two lists, create a list of tuples possible turns
  in the form (sum-die likelihood) the cartesian product is admittedly
  terrible for the state space
  (many possibilities need to be represented without being inferred)
  but works nicely for real-world-sided die"
  (let [all-turns (cartesian-product d1-list d2-list)]
    (map #(list
            (+ (first (first %)) (first (second %)))
            (* (second (first %)) (second (second %))))
         all-turns)))

(defn die-sums [d1 d2 number operator]
  "Given a number of faces of die 1 and die 2 and a number
  finds the probability of rolling < = > that number as the sum of
  the two die"
  
  (let [ d1-list (map #(list %  (/ 1 d1)) (range 1 (inc d1)))
        d2-list (map #(list % (/ 1 d2)) (range 1 (inc d2)))
        all-possible  (possible-combos d1-list d2-list)] 
    (/ 
      (count (filter #(operator (first %)  number) all-possible))
      (count all-possible))))



(defn problem-n [d1 d2 pivot sheet-placement total-turns num-in]
  "Roll a d1 faced and d2 faced die"
  
  "Choose a pivot number and make 'left' center' and 'right' columns on a sheet"
  
  "If the sum of the die faces is less than the pivot, put a tick in the left
  column, if it's equal put it in the center, and greater than in the right"
  
  "What is the probability of getting exactly num-in ticks
  in column sheet-placement = 'left' | 'center' | 'right' 
  after total-turns"
  
  (let 
    [ prob-in (cond
                (= sheet-placement "l")
                (die-sums d1 d2 pivot <)
                (= sheet-placement "c")
                (die-sums d1 d2 pivot =)
                (= sheet-placement "r")
                (die-sums d1 d2 pivot >))
     
     prob-out (- 1 prob-in)
     num-out  (- total-turns num-in)
     ways-of-winning (choose total-turns num-in)
     prob-n-in (Math/pow prob-in num-in)
     prob-rest-out (Math/pow prob-out num-out)]    
    (*
      ways-of-winning
      prob-n-in
      prob-rest-out)))

(defn guesses [tally-sheet pivot]
  (let [ place #(cond
                  (= "l" %)  (first tally-sheet)
                  (= "c" %)  (second tally-sheet)
                  (= "r" %)  (nth tally-sheet 2))
        total (reduce + tally-sheet)
       
        possibility-map
        {
         
         :d6-d6 (->> (map #(problem-n 6 6 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         :d6-d8 (->> (map #(problem-n 6 8 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         :d6-d12 (->> (map #(problem-n 6 12 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         :d6-d20 (->> (map #(problem-n 6 20 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         
         :d8-d8 (->> (map #(problem-n 8 8 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         :d8-d12 (->> (map #(problem-n 8 12 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         :d8-d20  (->> (map #(problem-n 8 20 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         
         
         :d12-d12 (->> (map #(problem-n 12 12 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         :d12-d20 (->> (map #(problem-n 12 20 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         
         :d20-d20 (->> (map #(problem-n 20 20 pivot % total (place %) ) (list "l" "c" "r")) (reduce +))
         }
        
        guess1 (apply max-key val possibility-map)
        possibility-map (dissoc possibility-map (key guess1)) 
        guess2 (apply max-key val possibility-map) 
        possibility-map (dissoc possibility-map (key guess2))
        guess3 (apply max-key val possibility-map)]
    
    (map first (list guess1 guess2 guess3))))


(defn key-word [n1 n2]
  "smaller goes first"
  (cond 
    (= n1 n2) (keyword (str "d"n1"-d"n2))
    (> n1 n2) (keyword (str "d"n2"-d"n1))
    (< n1 n2) (keyword (str "d"n1"-d"n2))))


(defn generate-sheet [pivot num-throws]
  (fn [die-pair]
    
    (let[d1 (first die-pair)
         d2 (second die-pair)
         d1-throws (sample (range 1 (inc d1)) :size num-throws)
         d2-throws (sample (range 1 (inc d2)) :size num-throws)
         sums (map + d1-throws d2-throws)
         l (count (filter #(< % pivot) sums))
         c (count (filter #(= % pivot) sums))
         r (count (filter #(> % pivot) sums))
         ]
      (list l c r))))

(defn game-outcome [pivot]
  (fn [game-tuple]
  (let [die-pair (apply key-word (first game-tuple))
        tally-sheet (second game-tuple)
        guess-list (guesses tally-sheet pivot)] ; hard codes pivot / die opts right now
    (cond 
      (= die-pair (first guess-list)) "win1"
      (= die-pair (second guess-list)) "win2"
      (= die-pair (nth guess-list 2)) "win3"
      :else "lose"))))


(defn choose-die [] (sample [6 8 12 20] :size 2))

(defn simulate-game [num-games num-throws pivot]
  (let [  die-choices (take num-games (repeatedly choose-die))
        tally-maker (generate-sheet pivot num-throws)
        games-with-die (map #(list % (tally-maker %)) die-choices)
        game-outcomes (map #((game-outcome pivot) %) games-with-die)
        win1 (count (filter #(= "win1" %) game-outcomes))
        win2 (count (filter #(= "win2" %) game-outcomes))
        win3 (count (filter #(= "win3" %) game-outcomes))
        lose (count (filter #(= "lose" %) game-outcomes))
        round-percent (fn [fract] (format "%.2f" (* 100 (float fract))))]
    (do 
      (println (str  "Chance of winning first " (/ win1 num-games)
                    (str " or " (round-percent(/ win1 num-games)) " percent"  )))
      (println (str  "Chance of winning second " (/ win2 num-games) " percent"
                    (str " or " (round-percent (/ win2 num-games)) " percent"  )))
      (println (str  "Chance of winning third "  (/ win3 num-games)
                    (str " or "  (round-percent(/ win3 num-games )) " percent"  )))
      (println (str  "Chance of losing round " (/ lose num-games)
                    (str " or " (round-percent (/ lose num-games))  " percent" )))
    
    
    
    (view (pie-chart ["Firsts" "Seconds" "Thirds" "Losses"] 
                     [(/ win1 num-games)
                      (/ win2 num-games)
                      (/ win3 num-games)
                      (/ lose num-games)])))))



(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(defn -main
  [num-games num-throws pivot]
  (simulate-game (parse-int num-games) (parse-int num-throws) (parse-int pivot)))
