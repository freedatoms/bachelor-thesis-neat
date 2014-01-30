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


(defn- excess
  [^Genome g1 ^Genome g2]
  (let [lg1   (last (:connection-genes g1))
        lg2   (last (:connection-genes g2))
        [a b] (if (> (:innov lg1) (:innov lg2))
                [lg2 g1]
                [lg1 g2])]
    (filter #(> (:innov %) (:innov a)) (:connection-genes b))))

(defn- excess-count
  [^Genome g1 ^Genome g2]
  (count (excess g1 g2)))

(defn- disjoint
  [^Genome g1 ^Genome g2]
  (let  [length (min (:innov (last (:connection-genes g1)))
                     (:innov (last (:connection-genes g2))))
         ]
    (loop [cur (first (:connection-genes g1))
           dis 0
           rg1 (rest (:connection-genes g1))
           rg2 (:connection-genes g2)]
        )
    ))


(defn- disjoint-count
  [^Genome g1 ^Genome g2]
  (-> (disjoint g1 g2)
      flatten
      count))

(defn- weight-diff
  [^Genome g1 ^Genome g2]
  1)

(defn delta
  [^Genome g1 ^Genome g2]
  (+ (/ (+ (* (excess-count g1 g2) @ep/c1)
           (* (disjoint-count g1 g2) @ep/c2))
        (max (count (:connection-genes g1))
             (count (:connection-genes g2))))
     (* (weight-diff g1 g2) @ep/c3)))



