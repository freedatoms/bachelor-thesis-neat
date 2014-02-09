(ns neat.species
  (:require [neat
             [individual :as ind]
             [genome :as genome]
             [operators :as op]
             [evolution-parameters :as ep]]))

(defrecord Species 
    [max-fitness
     last-fitness-increase
     representative
     expected-offspring
     individuals])

(def cur-pop
  (ref []))
(def sorted-pop
  (ref []))

(defn- find-species
  [gen]
  (loop [[gen2 & gens] (mapv :representative @cur-pop)
         i 0]
    (if gen2
      (if (< (genome/delta gen gen2) @ep/dt)
        i
        (recur gens (inc i)))
      i)))


(defn remove-worst-individuals-in-species
  []
  (dosync 
   (ref-set cur-pop
            (mapv (fn [spec]
                    (let [species (:individuals spec)]
                      (assoc spec :individuals
                             (take (Math/ceil (* (count species)
                                                 @ep/survival-rate-in-species))
                                   (sort-by :fitness > species)))))
                  @cur-pop))))

(defn- get-second-parent
  [species]
  (if (< (rand) @ep/interspecies-mating-prob)
    (first (:individuals (loop [i 0
                                spec (rand-nth @sorted-pop)]
                           (if (and (or (>= i 5)
                                        (not (= spec species)))
                                    (< 0 (count (:individuals spec))))
                             spec
                             (let [r (min (Math/abs (float (/ (.nextGaussian (java.util.Random.)) 4)))
                                          1.0)
                                   idx (Math/floor (+ (* r (- (count @sorted-pop) 1)) 0.5))]
                               (recur (inc i) (nth @sorted-pop idx)))))))
    (rand-nth (:individuals species))))

(defn- place-individual-in-species 
  [species individual generation]
  (let [ch-f (> (:raw-fitness individual) (:max-fitness species))]
    (assoc species
      :individuals (conj (:individuals species) individual)
      :max-fitness (if ch-f (:raw-fitness individual) (:max-fitness species))
      :last-fitness-increase (if ch-f generation (:last-fitness-increase species)))))

(defn- place-in-new-generation
  [individual generation]
  (let [idx (find-species (:genome individual))] 
    (if (== idx (count @cur-pop))
      (dosync
       (alter cur-pop conj (->Species (:raw-fitness individual) generation 
                                      (:genome individual)
                                       0.0 [individual])))
      (dosync 
       (alter cur-pop #(update-in % [idx] place-individual-in-species individual generation))))))


(defn- mutation 
  [individual]
  (ind/new-individual :genome (op/mutation (:genome individual))))

(defn- crossover 
  [ind1 ind2]
  (ind/new-individual :genome (op/crossover (:genome ind1)
                                            (:genome ind2)
                                            (- (:raw-fitness ind1)
                                               (:raw-fitness ind2)))))


(defn- reproduce-species
  [species generation]
  (when (< 0 (count (:individuals species)))
    (dotimes [eo (:expected-offspring species)]
      #_(when nil ;superchamp
          (if (or (< (rand) 0.8)
                  (== 0.0 @ep/add-connection-prob))
            (mutate-weights)
            (add-connection)))
      
      (place-in-new-generation (cond 
                                (> (:expected-offspring 
                                    (first (:individuals species)))  5) 
                                (first (:individuals species))

                                (< (rand) @ep/mutate-only-prob)
                                (mutation (rand-nth (:individuals species)))
                                
                                :else
                                (let [ind1 (rand-nth (:individuals species))
                                      ind2 (get-second-parent species)
                                      baby (crossover ind1 ind2)]
                                  (if (or (> (rand) @ep/mate-only-prob)
                                          (= ind1 ind2)
                                          (== 0.0 (genome/delta (:genome ind1)
                                                                (:genome ind2))))
                                    (mutation baby)
                                    baby)))
                               generation))))

(defn- dotimes-with-res 
  [upper-limit fun]
  (loop [i 0
         res []]
    (if (< i upper-limit)
      (recur (inc i) (conj res (fun i)))
      res)))

(defn- calculate-properties
  [population-size]
  (let [avg-fitness (/ (reduce + (map #(reduce + (map :raw-fitness (:individuals %))) @cur-pop))
                       population-size)]
    (dosync
     (alter cur-pop (fn [pop]
                      (mapv (fn [species]
                              (let [ninds (mapv #(assoc % 
                                                   :fitness (float (/ (:raw-fitness %)
                                                                      (count species)))
                                                   :expected-offspring (float (/ (/ 
                                                                                  (:raw-fitness %)
                                                                                  (count species))
                                                                                 avg-fitness)))
                                                (:individuals species))
                                    repr (if (< 0 (count ninds))
                                           (:genome (rand-nth ninds))
                                           (:representative species))]

                                (assoc species 
                                  :representative repr
                                  :individuals ninds
                                  :expected-offspring (reduce + (mapv :expected-offspring ninds)))))
                            pop))))))

(defn reproduce
  [generation population-size]
  (remove-worst-individuals-in-species)
  (dosync
   (ref-set sorted-pop (mapv #(update-in % [:expected-offspring] 
                                         (fn [x] (Math/floor x)))
                             (sort-by :max-fitness > @cur-pop)))
   (let [pop-eo (- population-size
                   (reduce + (map :expected-offspring @sorted-pop)))]
     
     (dotimes [idx pop-eo] ;;FIXME: even more prefer good species
       (alter sorted-pop update-in [(rem idx (count @sorted-pop)) :expected-offspring] inc)))
   (alter cur-pop #(mapv (fn [x] (assoc x :individuals [])) %)))
  
  (dorun (doseq [sp @sorted-pop]
           (reproduce-species sp generation)))
  (calculate-properties population-size))


(defn initialize
  [pop-size inputs outputs]
  (dorun (doseq [i (repeatedly pop-size #(ind/new-individual :inputs inputs
                                                             :outputs outputs))]
           (place-in-new-generation i 0))))


