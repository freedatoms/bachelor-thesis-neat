(ns neat.neural-net
  (:use [loom
         io
         alg
         graph]
        [neat
         evolution-parameters]))



(defn- prepare-genome
  [genome]
  (let [inc* (fn [x] (if x (inc x) 1))]
    (loop [[cur & rest] (filter :enabled? (:connection-genes genome))
           adj {}
           ins {}
           weights {}]
      (if cur
        (recur rest
               (update-in adj [(:in cur)] conj (:out cur))
               (update-in ins [(:out cur)] conj (:in cur))
               (assoc weights [(:in cur) (:out cur)] (:weight cur)))
        [adj ins weights]))))

(defn evaluate-neural-net
  [genome inputs & {:keys [context]
             :or {context {}}}]
  (let [[adj ins weights] (prepare-genome genome)
        outs (mapv :id (filter #(= :output (:type %)) (:node-genes genome)))]
    (if-let [sorted (topsort (digraph adj))]
      (loop [[n & r] sorted
             res (into {1 1} (mapv #(vector (:id %1) (@transfer-fun %2))
                                   (filter #(= :input (:type %)) (:node-genes genome)) inputs))]
        (if n
          (recur r (assoc res n (or (res n)
                                    (@transfer-fun
                                     (reduce + (mapv #(* (weights [% n]) (res %)) (ins n)))))))
          (mapv #(res %) outs)))
      (throw (Exception. "This neural net has cycles!")))))
