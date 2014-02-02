(ns neat.neural-net-test
    (:use [neat
         genome
         evolution-parameters
         [gene :as gene]
         graphviz-enabled
         operators
         neural-net]
        [clojure
         test]))


(defmacro time-of
  [expr]
  `(do (printf "'%s': "  (first '~expr))
       (time ~expr)))

(let [genome1 (->Genome (mapv (partial apply gene/->Node-gene)
                              [[1 :bias] [2 :input] [3 :input] [4 :output] [5 :hidden]])
                        (mapv (partial apply gene/->Connection-gene)
                              [[1 4 1.0 true  1]
                               [2 4 1.0 false 2]
                               [3 4 1.0 true  3]
                               [2 5 1.0 true  4]
                               [5 4 1.0 true  5]
                               [1 5 1.0 true  8]]))
      genome2 (->Genome (mapv (partial apply gene/->Node-gene)
                              [[1 :bias] [2 :input] [3 :input] [4 :output] [5 :hidden]
                               [6 :hidden]])
                        (mapv (partial apply gene/->Connection-gene)
                              [[1 4 0.3 true  1]
                               [2 4 0.4 false 2]
                               [3 4 0.5 true  3]
                               [2 5 0.6 true  4]
                               [5 4 0.7 false 5]
                               [5 6 0.8 true  6]
                               [6 4 0.9 true  7]
                               [3 5 0.1 true  9]
                               [1 6 0.2 true 10]]))]
  (deftest prepare-genome-test
    (is (= [{5 '(4), 3 '(4), 2 '(5), 1 '(5 4)} {5 2, 4 3}]
           (#'neat.neural-net/prepare-genome genome1)))
    (is (= [{6 '(4), 5 '(6), 3 '(5 4), 2 '(5), 1 '(6 4)} {6 2, 5 2, 4 3}]
           (#'neat.neural-net/prepare-genome genome2))))
  )
