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
  (let  [a (mapv :innov (:connection-genes g1))
         b (mapv :innov (:connection-genes g2))
         m (min (last a) (last b))
         a-set (set a)
         b-set (set b)]
    [(vec (filter #(and (<= % m) (not (b-set %))) a))
     (vec (filter #(and (<= % m) (not (a-set %))) b))]))

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



