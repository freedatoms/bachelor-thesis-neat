(ns neat.neural-net)



(defn- prepare-genome
  [genome]
  (let [inc* (fn [x] (if x (inc x) 1))]
    (loop [[cur & rest] (filter :enabled? (:connection-genes genome))
           adj {}
           in-no {}]
      (if cur
        (recur rest
               (update-in adj [(:in cur)] conj (:out cur))
               (update-in in-no [(:out cur)] inc*))
        [adj in-no]))))



(defn evaluate-neural-net
  [genome inputs & {:keys [context]
             :or {context {}}}]
  (let [pg (prepare-genome genome)]




    ))
