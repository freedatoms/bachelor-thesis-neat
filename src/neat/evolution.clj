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

(defn- init-generation
  []
  (reset! ep/gene-pool {})
  (species/swap-pop))

(defn- calculate-fitness
  [population]
  (dorun (doseq [i @population]
           (species/place-individual i)))
  (swap! population
         (fn [pop]
           (vec (sort-by :fitness >
                         (mapv (fn [i]
                                 (assoc i
                                   :fitness (species/fitness-share (:genome i) 
                                                                   (:raw-fitness i)))) pop))))))

(defn- mutate 
  [population]
  (swap! population 
         #(mapv (fn [i]
                  (ind/new-individual :genome (if (< (rand) @ep/mutation-prob) 
                                                (op/mutation (:genome i))
                                                (:genome i)))) %)))

(defn- avg 
  [coll]
  (/ (reduce + coll) (count coll)))

(defn- end-of-generation
  [generation highest-fitness highest-fitness-since pop frame]
  (printf "Generation: %d Species count: %d Best fitness: %f raw-fitness: %f highest-fit: %f since: %d \n"
          generation
          (count @species/last-pop)
          (:fitness (first @pop))
          (:raw-fitness (first @pop))
          highest-fitness
          highest-fitness-since
          )
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
   (ref-set species/cur-pop [])
   (ref-set species/last-pop []))
  (let [frame      (viz/create-frame "NEAT")
        population (atom (repeatedly pop-size #(ind/new-individual :inputs inputs
                                                                   :outputs outputs)))]
    (calculate-fitness population)
    (loop [generation 0
           highest-fitness 0.0
           highest-fitness-since 0]
      (if (not (end-criterium generation @population))
        (do (species/remove-worst-individuals-in-species)
            (species/crossover-species population)
            (mutate population)
            (species/swap-pop)
            (calculate-fitness population)
            (end-of-generation generation highest-fitness highest-fitness-since population frame)        
            (if (< highest-fitness (:raw-fitness (first @population)))
              (recur (inc generation) (:raw-fitness (first @population)) generation)
              (recur (inc generation) highest-fitness highest-fitness-since)))
        (do (let [succ (sort-by :raw-fitness > (filter :successful @population))]
              (if (< 0 (count succ))
                (view (first succ) (viz/create-frame
                                    (format "NEAT fitness: %f, raw-fitness: %f"
                                            (:fitness (first succ))
                                            (:raw-fitness (first succ)))))))
            generation)))))


