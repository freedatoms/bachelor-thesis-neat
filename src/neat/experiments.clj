(ns neat.experiments
  (:require [neat 
             [evolution2 :as evo]
             [evolution-parameters :as ep]
             [gui :as gui]
             [neural-net :as net]
             [image :as img]]
            [incanter
             [core :as co]
             [io :as io]                
             [datasets :as dat]]
            )
  (:import [cz.cvut.fit.thesis
            DiscreteMaze
            DiscreteMazeViewer
            MoveAction]
           javax.swing.JFrame))

                                        ;(set! *warn-on-reflection* true)

(def dataset-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/Implementace/datasets/")

(defn normalize-column*
  "Performs min-max normalization to interval [0,1]"
  [col]
  (let [min (apply min col)
        max (apply max col)
        diff (- max min)]
    (mapv (fn [x] (double (/ (- x min) diff))) col)))

(defn normalize-column 
  "Performs min-max normalization to interval [-1,1]"
  [col]
  (let [min (apply min col)
        max (apply max col)
        diff (- max min)]
    (mapv (fn [x] (double (+ -1 (* 2 (/ (- x min) diff))))) col)))

(defn normalize-columns
  [cols]
  (let [cls (loop 
                [[x & xs] (rest cols)
                 cs (mapv vector (first cols))]
              (if xs
                (recur xs (mapv conj cs x))
                (mapv conj cs x)))]
    (apply mapv (fn [& xs] (vec xs))
           (mapv normalize-column cls))))

(defn maxpos 
  [coll]
  (loop [[x & xs] coll
         max (first coll)
         maxI 0
         i 0]
    (if x
      (if (> x max)
        (recur xs x i (inc i))
        (recur xs max maxI (inc i)))
      maxI)))

(defn make-classification-fitness
  [inputs outputs]
  (fn [genome]
    (let [evals (mapv #(net/evaluate-neural-net-with-activation-cycles genome % 100) inputs)
          succ (mapv #(= (maxpos %1) %2) evals outputs)]
      {#_:fitness #_(double (max 0.000000001 
                                 (- 100 (/ (reduce + 
                                                   (mapv (fn [evaluated out] 
                                                           (/ (- (reduce #(+ %1 (* %2 %2)) 0 evaluated) 
                                                                 (* (nth evaluated out) (nth evaluated out)))
                                                              (count evaluated)))
                                                         evals outputs))
                                           (count evals)))))

       :solved (reduce #(and %1 %2) succ)
       :success-rate (/ (reduce #(+ %1 (if %2 1 0)) 0  succ)
                        (count succ))
       :fitness (double (Math/pow (/ (reduce #(+ %1 (if %2 1 0)) 0  succ)
                                     (count succ)) 2))})))

(defn- make-regression-fitness
  [input output success-threshold]
  (fn [genome]
    (let [evals (mapv #(net/evaluate-neural-net-with-activation-cycles genome % 100) input)
          succ (mapv (fn [e o] (reduce #(and %1 %2) true
                                       (mapv #(< (Math/abs ^float (- %1 %2)) success-threshold) e o)))
                     evals output)]
      {:fitness (double (max 0.000000001 
                             (- 100 (/ (reduce + 
                                               (mapv (fn [evaluated out] 
                                                       (reduce #(+ %1 (* %2 %2)) 0 
                                                               (mapv #(- %1 %2) evaluated out)))
                                                     evals output))
                                       (count evals)))))
       

       :solved (reduce #(and %1 %2) succ)
       :success-rate (- 1 (/ (reduce + 
                                     (mapv (fn [evaluated out] 
                                             (reduce #(+ %1 (Math/abs ^float %2)) 0 
                                                     (mapv #(- %1 %2) evaluated out)))
                                           evals output))
                             (count evals)))

       
       #_(/ (reduce #(+ %1 (if %2 1 0)) 0  succ)
            (count succ))
       ;; :fitness (Math/pow (float (/ (reduce #(+ %1 (if %2 1 0)) 0  succ)
       ;;                              (count succ)))
       ;;                    2)
       })))

(defn- maze-fitness
  "fitness used for maze"
  [genome]
  (let [m (DiscreteMaze.)
        a (.getNewActor m)
        s (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))
        not-a-wall (fn [c] (if (= c \w) 0.0 1.0))
        steps (loop [i 0]
                (if (< i(* 10 s))
                  (if (.move m a  
                             (case (maxpos (net/evaluate-neural-net-with-activation-cycles
                                            genome
                                            [i
                                             (float (/ (.shortestPathToTarget m ^int (.getX a) ^int (.getY a)) 
                                                       25))
                                             (not-a-wall (.lookForward m a))
                                             (not-a-wall (.lookLeft m a))
                                             (not-a-wall (.lookRight m a))]
                                            100))
                               1 MoveAction/TURN_LEFT
                               2 MoveAction/TURN_RIGHT
                               MoveAction/FORWARD))
                    i
                    (recur (inc i)))
                  i))]
    {:fitness (float (+ 100 (if (.isTarget m a)
                              (/ s steps)
                              (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))))))
     :solved (.isTarget m a)
     :success-rate (float (+ 100 (if (.isTarget m a)
                                   (/ s steps)
                                   (- (.shortestPathToTarget m ^int (.getX a) ^int (.getY a))))))}))


;; Problems
(defn xor-problem
  []
  (dosync
   (ref-set ep/input-count 2)
   (ref-set ep/output-count 1)
   (ref-set ep/weight-range [-26.0 26.0])
   (ref-set ep/fitness-fun 
            (fn [genome]
              {:fitness (Math/pow (max 0.000000001
                                       (- 4 
                                          (reduce + 
                                                  (mapv #(Math/abs ^float (- %2 
                                                                             (first (net/evaluate-neural-net-with-activation-cycles genome %1 10))))
                                                        [[0 0][1 0][0 1][1 1]]
                                                        [0 1 1 0])))) 2) 
               :solved? (and (== 0 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [0 0] 100))))
                             (== 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [1 0] 100))))
                             (== 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [0 1] 100))))
                             (== 0 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles 
                                                              genome [1 1] 100)))))
               :success-rate (/ (+ (- 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                                   genome [0 0] 100))))
                                   (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [1 0] 100)))
                                   (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                              genome [0 1] 100)))
                                   (- 1 (Math/round ^float (first (net/evaluate-neural-net-with-activation-cycles
                                                                   genome [1 1] 100)))))
                                4)})))
  (evo/evolution :name "xor"))




(defn iris
  []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "iris.data")))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 3, :weight-range [-1.0 1.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 2.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.3, :old-age 30, :clamp-weight-factor 1.0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.4, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 4, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (dosync
     (ref-set ep/fitness-fun (make-classification-fitness 
                              (normalize-columns (co/to-vect (co/sel dataset :cols [0 1 2 3])))
                              (mapv #(int %) (co/to-vect  (co/sel dataset :cols [4]))))))
    (evo/evolution :name "iris")))

(defn wine 
  []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "wine.data")))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 3, :weight-range [-5.0 5.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 2.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.3, :old-age 30, :clamp-weight-factor 0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.1, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 13, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (dosync 
     (ref-set ep/fitness-fun
              (make-classification-fitness (normalize-columns (co/to-vect (co/sel dataset :cols (range 1 14))))
                                           (mapv #(int (dec %)) (co/to-vect  (co/sel dataset :cols 0))))))
    (evo/evolution :name "wine")))

(defn glass 
  []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "glass.data")))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 7, :weight-range [-1.0 1.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 0.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.3, :old-age 30, :clamp-weight-factor 1.0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.2, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.3, :input-count 9, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1})
    (dosync 
     (ref-set ep/add-connection-prob 0.05)
     (ref-set ep/fitness-fun
              (make-classification-fitness (normalize-columns (co/to-vect (co/sel dataset :cols (range 1 10))))
                                           (mapv #(int (dec %)) (co/to-vect  (co/sel dataset :cols 10))))))
    (evo/evolution :name "glass")))


(defn regression-yacht []
  (let [dataset (co/to-matrix (io/read-dataset (str dataset-prefix "yacht_hydrodynamics.data") :delim \space))
        input (normalize-columns (co/to-vect (co/sel dataset :cols (range 0 6))))
        output (mapv vector (normalize-column* (co/to-vect (co/sel dataset :cols 6))))]
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.05, :c1 1.0, :output-count 1, :weight-range [-1.0 1.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 0.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 1.0, :old-age 30, :clamp-weight-factor 0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.05, :tournament-k 4, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 6, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (dosync
     (ref-set ep/fitness-fun (make-regression-fitness input output 0.001)))
    (evo/evolution :name "yacht")))


(defn regression-image []
  (let [dataset (img/image->matrix (str dataset-prefix "Lenna128.jpg"))
        maxw (reduce max (co/to-vect (co/sel dataset :cols 0)))
        maxh (reduce max (co/to-vect (co/sel dataset :cols 1)))
        input  (mapv #(double-array [(float (/ %1 maxw))
                                     (float (/ %2 maxh))]) 
                     (co/sel dataset :cols 0)
                     (co/sel dataset :cols 1))
        output (mapv vector (co/to-vect (co/sel dataset :cols 2)))
        frame (javax.swing.JFrame.)]
    (.add frame (javax.swing.JLabel.))
    (dosync
     (ref-set ep/population-size 1)
     (ref-set ep/generation-count 1000)
     (ref-set ep/input-count 2)
     (ref-set ep/output-count 1)
     (ref-set ep/visualize-genome-with [])
     (ref-set ep/target-species 5)
     (ref-set ep/add-node-prob 0.5)
     (ref-set ep/add-connection-prob 0.05)
     (ref-set ep/weight-range [-1.0 1.0])
     (ref-set ep/mutate-weights-perturb-sigma 1.0)
     (ref-set ep/clamp-weight-factor 1.0)
     (ref-set ep/fitness-fun (make-regression-fitness input output 0.001)))
    (img/show-image (img/matrix->image dataset)
                    :title "Original Image")
    (gui/show-options)
    (evo/evolution :name "image"
                   :do-with-best (fn [ind & _]
                                   (img/show-image 
                                    (img/matrix->image
                                     (co/matrix (for [x (range (inc maxw))
                                                      y (range (inc maxh))]
                                                  [x y (first
                                                        (net/evaluate-neural-net-with-activation-cycles 
                                                         (:genome ind)
                                                         [(float (/ x maxw)) (float (/ y maxh))] 
                                                         100))])))
                                    :frame frame
                                    :title "Evolved Image")))))
#_(regression-image)


(defn maze []
  (let [fr (JFrame.)]
    (.setSize fr 600 600)
    (dosync 
     (ref-set ep/fitness-fun maze-fitness))
    (gui/set-new-settings {:mate-by-choosing-prob 0.6, :add-node-prob 0.03, :c1 1.0, :output-count 3, :weight-range [-5.0 5.0], :c2 1.0, :dt 3.0, :connection-density 0.0, :mutation-prob 0.25, :survival-rate-in-species 0.2, :mutate-weights-perturb-sigma 2.5, :mate-only-prob 0.2, :mutate-weights-prob 0.8, :old-age-multiplier 0.2, :mutate-only-prob 0.25, :disable-in-crossover 0.75, :elitism true, :c3 0.2, :old-age 30, :clamp-weight-factor 1.0, :population-size 450, :stagnation-age 15, :target-species 0, :mutate-weights-perturb-prob 0.9, :crossover-prob 0.75, :dt-delta 0.4, :tournament-k 1, :interspecies-mating-prob 0.001, :min-elitism-size 5, :add-connection-prob 0.05, :input-count 5, :young-age 10, :visualize-genome-with [], :generation-count 100, :young-age-multiplier 1.2})
    (evo/evolution :name "maze"
                   :do-with-best
                   (fn [ind filename]
                     (let [m (DiscreteMaze.)
                           mv (DiscreteMazeViewer. 
                               m 
                               (fn [in]
                                 (double-array
                                  (net/evaluate-neural-net-with-activation-cycles 
                                   (:genome ind)
                                   (into [(first in) (float (/ (second in) 25))] (rest (rest in)))
                                   100))))
                           ]
                       (.setTitle fr (str "Maze fitness: " (:fitness ind)))
                       (.removeAll (.getContentPane fr))
                       (.add (.getContentPane fr) mv)
                       (when (not (.isShowing fr))
                         (.setVisible fr true))
                       (.saveImage mv (str filename "-maze.png") 800 800)
                       (.revalidate fr))))))

(dotimes [_ 20]
     (prn :iris----------------------------------------------)
     (iris)
     (prn :wine----------------------------------------------)
     (wine)
    ;; (prn :yacht---------------------------------------------)
    ;; (regression-yacht)
   ;; (prn :maze----------------------------------------------)
    ;;(maze)
    #_(prn :glass---------------------------------------------)
    #_(glass))
