(ns neat.evolution
  (:require [rhizome 
             [viz :as viz]]
            [neat 
             [individual :as ind]
             [species :as species] 
             [neural-net :as net]
             [operators :as op]
             [evolution-parameters :as ep]])
  (:use  [neat
          graphviz-enabled] ))

(defn- avg 
  [coll]
  (/ (reduce + coll) (count coll)))



(defn- end-of-generation
  [generation pop frame]
  (printf "Generation: %d Species count: %d Best fitness: %f raw-fitness: %f  \n"
          generation
          (count @species/cur-pop)
          (:fitness (first @pop))
          (:raw-fitness (first @pop)))
   #_(view (first @pop) frame
        (format "NEAT generation: %d, #species: %d, fitness: %f, raw-fitness: %f, successful: %s"
                generation
                (count @species/last-pop)
                (:fitness (first @pop))
                (:raw-fitness (first @pop))
                (:successful (first @pop)))))

(defn evolve
  [inputs outputs pop-size end-criterium]
  (reset! ep/innovation-number 0)
  (reset! ep/gene-pool {})
  (dosync
   (ref-set species/cur-pop []))
  (let [frame      (viz/create-frame "NEAT")
        population (atom [])]
    (species/initialize pop-size inputs outputs)
    (loop [generation 0]
      (when (not (end-criterium generation @population))
        (species/reproduce generation pop-size)
        (reset! population (vec (sort-by :raw-fitness (mapcat :individuals @species/cur-pop))))
        (end-of-generation generation 
                           population
                           frame)        
        
        (recur (inc generation))))
    #_(let [succ (sort-by :raw-fitness > (filter :successful @population))]
      (if (< 0 (count succ))
        (view (first succ) (viz/create-frame
                            (format "NEAT fitness: %f, raw-fitness: %f"
                                    (:fitness (first succ))
                                    (:raw-fitness (first succ)))))))))


(evolve 2 1 150
          (fn [gen pop]
            (or
             (< 0 (count (filter :successful pop)))
             (> gen 1000))))
