(ns neat.evolution-parameters)


(def c1
  "Excess gene importance in delta function"
  (ref 1.0))
(def c2
  "Disjoint gene importance in delta function"
  (ref 1.0))
(def c3
  "Weight difference importance in delta function"
  (ref 0.4))
(def dt
  "Delta threshold for species separation"
  (ref 3.0))

;; Mutation
(def mutate-weights-prob
  "Probability"
  (ref 0.8))
(def mutate-weights-perturb-prob
  "Probability of weight perturbation (= P(perturb|mutate weights))"
  (ref 0.9))
(def mutate-weights-perturb-sigma
  "Sigma of mutate weights perturbation"
  (ref 1.0))
(def add-connection-prob
  "Probability of add-connection mutation"
  (ref 0.05))
(def add-node-prob
  "Probability of add-node mutation"
  (ref 0.03))


;; Crossover
(def crossover-prob
  "Probability of crossover"
  (ref 0.75))
(def interspecies-mating-prob
  "Probability of interspecies mating"
  (ref 0.001))





;;; internal
(def weight-range
  "Lower and upper bound"
  (ref [-1.0 1.0]))

(def innovation-number
  "Used for historical markings"
  (atom 0))

(def gene-pool
  "used during add-node and add-connection mutations to enhance gene-matching"
  (atom {}))


;; Functions
(defn rand-weight
  "Random floating-point number from range specified in weight-range ref or in args"
  ([]
     (rand-weight (first @weight-range) (second @weight-range)))
  ([lo hi]
     (+ (rand (- hi lo)) lo)))


