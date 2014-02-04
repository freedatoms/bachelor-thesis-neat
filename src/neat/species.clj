(ns neat.species
  (:require [neat
             [genome :as genome]
             [evolution-parameters :as ep]]))

(defrecord Species
    [sample
     inds])

(def last-pop
  (ref []))

(def cur-pop
  (ref [[1 2 3] [4 5 6] [7 8 9]]))

(defn swap-pop []
  (dosync
   (ref-set last-pop (mapv rand-nth @cur-pop))
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
       (alter cur-pop update-in [idx] conj gen))
      (dosync
       (alter cur-pop update-in [idx] conj gen)))))


(defn fitness-share
  [genome fitness]
  (/ fitness
     (count (nth @cur-pop (find-species genome)))))
