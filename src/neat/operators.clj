(ns neat.operators
  (:require [neat
             [gene :as gene]
             [genome :as genome]]
            )
  (:use [neat evolution-parameters])
  (:import [java.util Random]))

(defn- add-node
  "Takes genome and replace existing connection with node and two connections."
  [^neat.genome.Genome g]
  (let [len  (count (:connection-genes g))
        ng   (inc (count (:node-genes g)))
        gene (loop [cg (rand-int len)
                    cnt 0]
               (if (:enabled? (nth (:connection-genes g) cg))
                 cg
                 (if (> cnt len)
                   nil
                   (recur (rand-int len) (inc cnt)))))]
    (if-not gene
      g
      (let [cg   (nth (:connection-genes g) gene)
            node-genes (conj (:node-genes g) (gene/->Node-gene ng :hidden))
            conn-genes (assoc-in (:connection-genes g) [:connection-genes cg :enabled?] false)
            c1 (gene/->Connection-gene (:in cg) ng 1.0 true (or (@gene-pool [(:in cg) ng])
                                                                (swap! innovation-number inc)))
            c2 (gene/->Connection-gene ng (:out cg) (:weight cg) true (or (@gene-pool [ng (:out cg)])
                                                                          (swap! innovation-number inc)))]
        (swap! gene-pool assoc
               [(:in c1) (:out c1)] (:innov c1)
               [(:in c2) (:out c2)] (:innov c2))
        (genome/->Genome node-genes
                    (into conn-genes [c1 c2]))))))

(defn- new-conn
  "Creates new tuple first item is from ins second from outs."
  [ins outs]
  (loop [in (rand-nth ins)
         out (rand-nth outs)]
    (if (= in out)
      (recur (rand-nth ins) (rand-nth outs))
      [in out])))

(defn- add-connection
  "Takes genome and adds one connection gene."
  [^neat.genome.Genome g]
  (let [in      (count (filter #{:input :bias} (map :type (:node-genes g))))
        out     (count (filter #{:output} (map :type (:node-genes g))))
        total   (count (:node-genes g))
        ins     (into (vec (range 1 (inc in))) (range (+ in out 1) (inc total)))
        outs    (vec (range (inc in) (inc 10)))
        conns   (set (map #(vector (:in %) (:out %)) (:connection-genes g)))
        nc      (loop [conn (new-conn ins outs)
                       cnt  0]
                  (if (conns conn)
                    (if (< cnt (* total total))
                      (recur (new-conn ins outs) (inc cnt))
                      nil)
                    conn))]
    (if-not nc
      g
      (let [node-genes (:node-genes g)
            c (gene/->Connection-gene (first nc) (second nc) (rand-weight) true (or (@gene-pool nc)
                                                                                     (swap! innovation-number inc)))
            conn-genes (conj (:connection-genes g) c)]
        (swap! gene-pool assoc nc (:innov c))
        (genome/->Genome node-genes conn-genes)))))

(defn- mutate-weights
  "With probability of @mutate-weights-perturb-prob uniformly perturbs weight
   otherwise replaces weight random number from uniform distribution."
  [^neat.genome.Genome g]
  (let [rnd (Random.)
        randf (fn [w] (if (< (rand) @mutate-weights-perturb-prob)
                          (+ (.nextGaussian rnd) w)
                          (rand-weight)))]
    (genome/->Genome (:node-genes g) (mapv #(update-in  % [:weight] randf) (:connection-genes g)))))

(defn- prob-call
  "Call (f arg) with probability prob otherwise returs arg."
  [prob f arg]
  (if (< (rand) prob)
    (f arg)
    arg))

(defn mutation
  "Mutate genome g.
   Mutates weight with probability of @mutate-weights-prob.
   Adds connection with probability of @add-connection-prob.
   Adds node with probability of @add-node-prob."
  [^neat.genome.Genome g]
  (->> g   
       (prob-call @mutate-weights-prob mutate-weights)
       (prob-call @add-connection-prob add-connection)
       (prob-call @add-node-prob add-node)))

(defn crossover
  [^neat.genome.Genome g1 ^neat.genome.Genome g2]
  )

