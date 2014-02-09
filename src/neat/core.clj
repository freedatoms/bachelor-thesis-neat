(ns neat.core
  (:gen-class)
  (:require [neat 
             [evolution-parameters :as ep]
             [neural-net :as net]
             [species :as species]
             [evolution :as e]]))


(def fitness-eval (atom 0))

(dosync 
 (ref-set ep/c1 1.0)
 (ref-set ep/c2 1.0)
 (ref-set ep/c3 0.4)
 (ref-set ep/dt 3.0)
 (ref-set ep/add-node-prob 0.03)
 (ref-set ep/add-connection-prob 0.05)
 (ref-set ep/mutate-weights-prob 0.8)
 (ref-set ep/mutate-weights-perturb-prob 0.9)
 (ref-set ep/mutate-only-prob 0.025)
 (ref-set ep/disable-in-crossover 0.75)
 (ref-set ep/mate-only-prob 0.02)
 (ref-set ep/weight-range [-1.0 1.0])
 (ref-set ep/mutate-weights-perturb-sigma 1.0)
 (ref-set ep/interspecies-mating-prob 0.001)
 (ref-set ep/survival-rate-in-species 0.2)
 (ref-set ep/connection-density -1.0)
 (ref-set ep/visualize-genome-with [])
 (ref-set ep/fitness-fun 
          (fn [genome]
            (swap! fitness-eval inc)
            [(Math/pow (- 4 
                          (reduce + 
                                  (mapv #(Math/abs (- %2 
                                                      (first (net/evaluate-neural-net-with-activation-cycles genome %1 10))))
                                        [[0 0][1 0][0 1][1 1]]
                                        [0 1 1 0]))) 2) 
             (and (== 0 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [0 0] 100))))
                  (== 1 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [1 0] 100))))
                  (== 1 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [0 1] 100))))
                  (== 0 (Math/round (first (net/evaluate-neural-net-with-activation-cycles genome [1 1] 100)))))])))








#_(e/evolve 2 1 150
          (fn [gen pop]
            (or
             (< 0 (count (filter :successful pop)))
             (> gen 1000))))

(def fitness-eval (atom 0))
(def d (mapv (fn [_]
               (reset! fitness-eval 0)
               (e/evolve 2 1 150
                         (fn [gen pop]
                           (or
                            (< 0 (count (filter :successful pop)))
                            (> gen 1000))))
               @fitness-eval) (range 5)))

(defn std [coll]
  (let [mu (float (/ (reduce + coll) (count coll)))]
    (Math/sqrt (float (/ (reduce + (mapv #(Math/pow (- %1 mu) 2) coll))
                         (count coll))))))

(prn :avg (float (/ (reduce + d) (count d))))
(prn :std (std d))
(prn :min (apply min d))
(prn :max (apply max d))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))
