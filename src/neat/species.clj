(ns neat.species
  (:require [neat
             [individual :as ind]
             [genome :as genome]
             [operators :as op]
             [evolution-parameters :as ep]]))

(def last-pop
  (ref []))

(def cur-pop
  (ref []))

(defn- get-representants
  []
  (loop [[cp & cpr] @cur-pop
         [lp & lpr] @last-pop
         res []]
    (if cp
      (recur cpr lpr (conj res (or (if (> (count cp) 0) (:genome (rand-nth cp))) lp)))
      res)))

(defn swap-pop []
  (dosync
   (ref-set last-pop (get-representants))
   (ref-set cur-pop (vec (repeat (count @last-pop) (list))))))

(defn- find-species
  [gen]
  (loop [[gen2 & gens] @last-pop
         i 0]
    (if gen2
      (if (< (genome/delta gen gen2) @ep/dt)
        i
        (recur gens (inc i)))
      i)))

(defn place-individual
  [ind]
  (let [gen (:genome ind)
        idx (find-species gen)]
    (if (= idx (count @last-pop))
      (dosync
       (alter last-pop assoc idx gen)
       (alter cur-pop update-in [idx] conj ind))
      (dosync
       (alter cur-pop update-in [idx] conj ind)))))

(defn fitness-share
  [genome fitness]
   (/ fitness
      (count (nth @cur-pop (find-species genome)))))


(defn remove-worst-individuals-in-species
  []
  (dosync 
   (ref-set cur-pop
            (mapv (fn [species]
                    (take (Math/ceil (* (count species)
                                        @ep/survival-rate-in-species))
                          (sort-by :fitness > species)))
                  @cur-pop))))

(defn- rand-species
  []
  (loop [spec (rand-nth @cur-pop)]
    (if (< 0 (count spec))
      spec
      (recur (rand-nth @cur-pop)))))

(defn- do-crossover 
  "With crossover-prob crossovers two individuals from same species with 1-interspecies-mating-prob
   and from different species with interspecies-mating-prob.
   With 1-crossover-prob returns a random individual from specified species."
  [species]
  (if (< (rand) @ep/crossover-prob)
    (let [ind1 (rand-nth species)
          ind2 (rand-nth (if (< (rand) @ep/interspecies-mating-prob)
                           (rand-species)
                           species))]
      (let [gene (op/crossover (:genome ind1) (:genome ind2) (- (:fitness ind1)
                                                                (:fitness ind2)))]
        (ind/new-individual :genome gene)))
    (rand-nth species)))

(defn crossover-species
  [population]
  (swap! population #(let [pop-size (count %)]
                       (loop [i 0 
                              res []]
                         (if (< i pop-size)
                           (recur (inc i) (conj res (do-crossover (rand-species))))
                           res)))))
