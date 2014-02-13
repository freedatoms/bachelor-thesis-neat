(ns neat.experiments
  (:require [neat 
             [evolution2 :as evo]
             [evolution-parameters :as ep]
             [neural-net :as net]]
            [incanter
             [core :as co]
             [io :as io]                
             [datasets :as dat]]))




(defn xor-problem
  []
  (dosync
   (ref-set ep/fitness-fun 
            (fn [genome]
              [(Math/pow (max 0.000000001
                              (- 4 
                                 (reduce + 
                                         (mapv #(Math/abs (- %2 
                                                             (first (net/evaluate-neural-net-with-activation-cycles genome %1 10))))
                                               [[0 0][1 0][0 1][1 1]]
                                               [0 1 1 0])))) 2) 
               (and (== 0 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [0 0] 100))))
                    (== 1 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [1 0] 100))))
                    (== 1 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [0 1] 100))))
                    (== 0 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [1 1] 100)))))])))
  (evo/evolution))

(defn maxpos 
  [coll]
  (loop [[x & xs] coll
         max (first coll)
         maxI 0
         i 0]
    (if x
      (if (> x max)
        (recur xs x i (inc i))
        (recur xs max maxI (inc i)))
      maxI)))

(defn make-classification-fitness
  [inputs outputs]
  (fn [genome]
    (let [evals (mapv #(net/evaluate-neural-net-with-activation-cycles genome % 100) inputs)]
      [(double (max 0.000000001 (- 100 (/ (reduce + (mapv (fn [evaluated out] 
                                                            (/ (- (reduce #(+ %1 (* %2 %2)) 0 evaluated) 
                                                                  (* (nth evaluated out) (nth evaluated out)))
                                                               (count evaluated)))
                                                          evals outputs))
                                          (count evals)))))
       (reduce #(and %1 %2) 
               (mapv #(= (maxpos )
                         %2) evals outputs))])))
