(ns neat.evolution
  (:require [rhizome 
             [viz :as viz]]
            [neat 
             [individual :as ind]
             [species :as species] 
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
  #_(prn (mapv (juxt :raw-fitness :fitness) 
        ((fn [pop]
           (vec (sort-by :fitness >
                         (mapv #(assoc % :fitness
                                       (species/fitness-share 
                                        (:genome %)
                                        (:raw-fitness %))) pop)))) @population)))
  (swap! population
         (fn [pop]
           (vec (sort-by :fitness >
                         (mapv #(assoc % :fitness
                                       (species/fitness-share 
                                        (:genome %)
                                        (:raw-fitness %))) pop))))))

(defn- mutate 
  [population]
  (swap! population 
         #(mapv (fn [i]
                  (if (< (rand) @ep/mutation-prob) 
                    (let [gen (op/mutation (:genome i))]
                      (ind/new-individual :genome gen))
                    i)) %)))


(defn- end-of-generation
  [generation pop frame]
  (printf "Generation: %d Species count: %d Best fitness: %f raw-fitness: %f\n"
          generation
          (count @species/last-pop)
          (:fitness (first @pop))
          (:raw-fitness (first @pop))
          )
  (view (first @pop) frame
        (format "NEAT generation: %d, #species: %d, fitness: %f, raw-fitness: %f"
                generation
                (count @species/last-pop)
                (:fitness (first @pop))
                (:raw-fitness (first @pop)))))

(defn evolve
  [inputs outputs pop-size end-criterium]
  (reset! ep/innovation-number 0)
  (reset! ep/gene-pool {})
  (dosync
   (ref-set species/cur-pop [])
   (ref-set species/last-pop [])
   (ref-set ep/fitness-fun (fn [_] (rand 10))))
  (let [frame      (viz/create-frame "NEAT")
        population (atom (repeatedly pop-size #(ind/new-individual :inputs inputs
                                                                   :outputs outputs)))]
    (calculate-fitness population)
    (loop [generation 0]
      (when (not (end-criterium generation))
        ;(prn (mapv :fitness @population))
        (species/remove-worst-individuals-in-species)
        (species/crossover-species population)
        (mutate population)
        (species/swap-pop)
        (calculate-fitness population)
        (end-of-generation generation population frame)        
        (recur (inc generation))))))

(dosync 
 ;; (ref-set ep/add-node-prob 2.0)
 ;; (ref-set ep/add-connection-prob 2.0)
 ;; (ref-set ep/mutation-prob 2.0)
 ;; (ref-set ep/mutate-weights-prob 2.0)
 ;; (ref-set ep/crossover-prob 2.0)
 ;; (ref-set ep/interspecies-mating-prob 0.5)
 (ref-set ep/connection-density 0.0)
 (ref-set ep/visualize-genome-with []))

(evolve 5 3 1000 #(> % 500))
