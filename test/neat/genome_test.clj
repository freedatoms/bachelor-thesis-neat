(ns neat.genome-test
  (:use [neat
         genome
         [gene :as gene]]
        [clojure
         test]))


                        

(let [genome1 (->Genome (mapv (partial apply gene/->Node-gene)
                              [[1 :input] [2 :input] [3 :input] [4 :output] [5 :hidden]])
                        (mapv (partial apply gene/->Connection-gene)
                              [[1 4 1.0 true  1]
                               [2 4 1.0 false 2]
                               [3 4 1.0 true  3]
                               [2 5 1.0 true  4]
                               [5 4 1.0 true  5]
                               [1 5 1.0 true  8]]))
      genome2 (->Genome (mapv (partial apply gene/->Node-gene)
                              [[1 :input] [2 :input] [3 :input] [4 :output] [5 :hidden]])
                        (mapv (partial apply gene/->Connection-gene)
                              [[1 4 1.0 true  1]
                               [2 4 1.0 false 2]
                               [3 4 1.0 true  3]
                               [2 5 1.0 true  4]
                               [5 4 1.0 false 5]
                               [5 6 1.0 true  6]
                               [6 4 1.0 true  7]
                               [3 5 1.0 true  9]
                               [1 6 1.0 true 10]]))]
  (deftest excess
    (is  (= [9 10] (mapv :innov (#'neat.genome/excess genome1 genome2)))))
  (deftest excess-count
    (is  (= 2 (#'neat.genome/excess-count genome1 genome2))))
  (deftest disjoint
    (is (= [[6 7] [8]] (#'neat.genome/disjoint genome1 genome2))))
  (deftest disjoint-count
    (is (= 3 (#'neat.genome/disjoint-count genome1 genome2)))))

