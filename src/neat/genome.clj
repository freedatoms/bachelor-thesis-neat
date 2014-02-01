(ns neat.genome
  (:require [neat
             [gene :as gene]
             [evolution-parameters :as ep]]))

(defrecord Genome
    [
     node-genes
     connection-genes
     ])

(defn initial-genome
  [input-count output-count]
  (let [tmp  (+ 2 input-count)
        inputs (into [(gene/->Node-gene 1 :bias)]
                     (mapv #(gene/->Node-gene % :input) (range 2 tmp)))
        nodes (into inputs
                    (mapv #(gene/->Node-gene (+ % tmp) :output)
                          (range output-count)))
        innov (atom 0)
        connections (vec (for [in inputs
                               out (nthrest nodes (dec tmp))]
                           (gene/new-connection-gene (:id in) (:id out) 1 true 
                                                     (swap! innov inc))
                           ))]
    (->Genome nodes connections)))

(defn match-genes
  [^Genome g1 ^Genome g2]
  (loop [i1 (first (:connection-genes g1))
         i2 (first (:connection-genes g2))
         r1 (next (:connection-genes g1))
         r2 (next (:connection-genes g2))
         res []]
    (if (or i1 i2)
      (cond
       (or (not (and i1 i2)) (= (:innov i1) (:innov i2))) (recur (first r1)
                                                                 (first r2)
                                                                 (next r1)
                                                                 (next r2)
                                                                 (conj res [i1 i2]))
       (> (:innov i1) (:innov i2))  (recur i1
                                           (first r2)
                                           r1
                                           (next r2)
                                           (conj res [nil i2]))
       (< (:innov i1) (:innov i2))  (recur (first r1)
                                           i2
                                           (next r1)
                                           r2
                                           (conj res [i1 nil])))
      res)))

(defn- excess
  [^Genome g1 ^Genome g2]
  (let [minG (min (:innov (last (:connection-genes g1)))
               (:innov (last (:connection-genes g2))))
        matchG (match-genes g1 g2)]
    (mapv (fn [[x y]] (or x y)) (filter (fn [[x y]] (> (:innov (or x y)) minG)) matchG))))

(defn- excess-count
  [^Genome g1 ^Genome g2]
  (count (excess g1 g2)))

(defn- disjoint
  [^Genome g1 ^Genome g2]
  (let [minG (min (:innov (last (:connection-genes g1)))
               (:innov (last (:connection-genes g2))))
        matchG (match-genes g1 g2)]
    (mapv (fn [[x y]] (or x y)) (filter (fn [[x y]] (and (not (and x y))
                                                       (<= (:innov (or x y)) minG)))
                                       matchG))))

(defn- disjoint-count
  [^Genome g1 ^Genome g2]
  (count (disjoint g1 g2)))

(defn- weight-diff
  [^Genome g1 ^Genome g2]
  (let [ab-set (clojure.set/intersection (set (mapv :innov (:connection-genes g1)))
                                         (set (mapv :innov (:connection-genes g2))))
        a (vec (filter #(ab-set (:innov %)) (:connection-genes g1)))
        b (vec (filter #(ab-set (:innov %)) (:connection-genes g2)))]
    (/ (reduce + (map #(Math/abs (- (:weight %1) (:weight %2))) a b))
       (count a))))

(defn delta
  [^Genome g1 ^Genome g2]
  (+ (/ (+ (* (excess-count g1 g2) @ep/c1)
           (* (disjoint-count g1 g2) @ep/c2))
        (max (count (:connection-genes g1))
             (count (:connection-genes g2))))
     (* (weight-diff g1 g2) @ep/c3)))




