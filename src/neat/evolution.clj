(ns neat.evolution
  (:use [neat
         [individual :as ind]
         [species :as species]
         [evolution-parameters :as ep]
         graphviz-enabled]))

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
           (sort-by :fitness >
                    (mapv #(assoc % :fitness
                                  (species/fitness-share (:genome %) (:raw-fitness %))) pop)))))







(defn evolve
  [inputs outputs pop-size end-criterium]
  (reset! ep/innovation-number 0)
  (reset! ep/gene-pool {})
  (dosync
   (ref-set species/cur-pop [])
   (ref-set species/last-pop [])
   (ref-set fitness-fun (fn [_] (rand 10))))
  (let [frame      (viz/create-frame "NEAT")
        population (atom (repeatedly pop-size #(ind/new-individual :inputs inputs
                                                                   :outputs outputs)))]
    (loop [generation 0]
      (when (end-criterium generation)
          (calculate-fitness population)
         ; (selection population)
         ; (mating population)
        (printf "Generation: %d Species count: %d Best fitness: %f raw-fitness: %f\n"
                generation
                (count @species/last-pop)
                (:fitness (first @population))
                (:raw-fitness (first @population))
                )
        (view (first @population) frame
              (format "NEAT generation: %d, #species: %d, fitness: %f, raw-fitness: %f"
                      generation
                      (count @species/last-pop)
                      (:fitness (first @population))
                      (:raw-fitness (first @population))))
        (species/swap-pop)
        (recur (inc generation))))
    ))

(evolve 5 3 100 #(< % 2))
