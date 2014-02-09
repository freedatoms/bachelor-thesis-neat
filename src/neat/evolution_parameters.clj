(ns neat.evolution-parameters)

(defrecord Option
    [name
     var
     description 
     type
     range])

(def options (atom []))

(defmacro defoption
  "Creates an option"
  [name description default-value 
   &{:keys [type range title]
     :or {type :float}}]
  (let [titl (or title (str name))
        rang (or range (case type
                         :probability [0.0 1.0]
                         nil))]
    `(do
       (def ~name ~description (ref ~default-value))
       (swap! options conj (Option. ~titl ~name ~description ~type ~rang)))))

;; Evolution parameters
(defoption c1
  "Excess gene importance in delta function"
   1.0
  :type :float)
(defoption c2
  "Disjoint gene importance in delta function"
   1.0
  :type :float)
(defoption c3
  "Weight difference importance in delta function"
   0.4
  :type :float)
(defoption dt
  "Delta threshold for species separation"
   3.0
  :type :float)
(defoption transfer-fun
  "Activation(=tranfer) function"
   (fn [x] (/ 1 (+ 1 (Math/exp (* -4.9 x)))))
  :type :function
  :title "Transfer function")
(defoption survival-rate-in-species 
  "Specifies how many individui should survive the elimination 
   of the lowest performing members in every species. 

   Number of surviving members of species is ceil(#ind * survival rate), 
   this way at least one individual survives."
   0.85
  :type :probability)
(defoption weight-range
  "Lower and upper bound"
  [-1.0 1.0]
  :type :range)

;; Creation
(defoption connection-density
  "How many connections are created in initial population.
   If set to 0 only max(in+1,out) connections will be created"
   0.1
  :type :probability)
(defoption fitness-fun
  "Fitness function"
  (fn [&_] (throw (Exception. "Fitness function is not set")))
  :type :function)

;; Mutation
(defoption mutation-prob
  "Probability of mutation"
  0.5
  :type :probability)
(defoption mutate-weights-prob
  "Probability of mutating weights"
  0.8
  :type :probability)
(defoption mutate-weights-perturb-prob
  "Probability of weight perturbation (= P(perturb|mutate weights))"
  0.9
  :type :probability)
(defoption mutate-weights-perturb-sigma
  "Sigma of mutate weights perturbation"
  1.0
  :type :probability)
(defoption add-connection-prob
  "Probability of add-connection mutation"
  0.05
  :type :probability)
(defoption add-node-prob
  "Probability of add-node mutation"
  0.03
  :type :probability)


;; Crossover
(defoption crossover-prob
  "Probability of crossover"
  0.75
  :type :probability)
(defoption interspecies-mating-prob
  "Probability of interspecies mating"
  0.001
  :type :probability)
(defoption disable-in-crossover
  "Probability of being disabled when either parent gene is"
  0.75
  :type :probability)

;; Visualization
(defoption visualize-genome-with
  "Set to [] if you are only interested in shape of the neural net"
  [:conn-gene :node-gene]
  :type :any-of
  :range [:conn-gene :node-gene])

;;; internal
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


