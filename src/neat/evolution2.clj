(ns neat.evolution2
  (:require [neat
             [population2 :as pop]
             [evolution-parameters :as ep]
             [gui :as gui]]
            [clojure
             [inspector :as ins]])
  (:use [neat
         graphviz-enabled]))


(require '[neat [neural-net :as net]])
(dosync
 (ref-set ep/fitness-fun 
          (fn [genome]
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

(defn- mean 
  [coll]
  (if (>  (count coll) 0)
    (/ (reduce + coll)
       (count coll))
    0.0))

(defn- std [coll]
  (Math/sqrt (float (/ (reduce + (mapv #(Math/pow (- %1 (mean coll)) 2) coll))
                       (count coll)))))

(defn- median
  [coll]
  (nth (sort coll) (Math/floor (float (/ (count coll) 2)))))

(defn- print-stats 
  [coll]
  (let [evals (mapv (partial * @ep/population-size) coll)]
    (prn :mean (mean evals)
         :std (std evals)
         :min (apply min evals)
         :max (apply max evals)
         :median (median evals))))


(defn evolution
  []
  (reset! ep/innovation-number 0)
  (reset! ep/species-count 0)
  (let [population (atom (pop/new-population))]
    (while (not (:solved-at @population))
      (let [stats (last (:stats (pop/evolve population)))]
        #_(printf "Generation: %d Species count: %d solved: %d Best fitness: %s Avg fitness: %f dt: %f innov: %d\n"
                (:generation stats)
                (count (:species stats))
                (count (:solutions stats))
                (apply max (mapv :max-fitness (:species stats)))
                (mean (mapv  :avg-fitness (:species stats)))
                (:current-dt stats)
                @ep/innovation-number)))
    #_(ins/inspect-tree (first (:solutions (last (:stats @population)))))
    #_(view (first (:solutions (last (:stats @population)))))
    (:generation @population)))
(require '[neat [individual2 :as ind]])


(defn -main 
  [& args]
  (gui/show-options)
  (dosync
   (ref-set ep/weight-range [-20.0 20.0]))
  (dorun (doseq [n [10 50 100]
                 r [[-8.0 8.0]
                    [-10.0 10.0]
                    [-12.0 12.0]
                    [-14.0 14.0]
                    [-16.0 16.0]
                    [-18.0 18.0]
                    [-20.0 20.0]
                    [-22.0 22.0]
                    [-24.0 24.0]
                    [-26.0 26.0]
                    [-28.0 28.0]
                    [-30.0 30.0]
                    [-32.0 32.0]
                    [-34.0 34.0]
                    [-36.0 36.0]
                    [-38.0 38.0]
                    [-40.0 40.0]]]
           (dosync
            (ref-set ep/weight-range r))
           (print n :=> r)
           (def d (mapv (fn [_] (evolution)) (range n)))
           (print-stats d))))
