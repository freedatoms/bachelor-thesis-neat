(ns neat.neural-net
  (:use [loom
         io
         alg
         graph]
        [neat
         evolution-parameters]))

(set! *warn-on-reflection* true)

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
             res (into {1 1} (mapv #(vector (:id %1) %2)
                                   (filter #(= :input (:type %)) (:node-genes genome)) inputs))]
        (if n
          (recur r (assoc res n (or (res n)
                                    (@transfer-fun
                                     (reduce + (mapv #(* (weights [% n]) (res %)) (ins n)))))))
          (mapv #(res %) outs)))
      [0] #_(throw (Exception. "This neural net has cycles!")))))

(defn- doubles-map-to-with-diff
  [fun ^doubles from ^doubles to
   & {:keys [starting-index]
      :or {starting-index 0}}]
  (let [len (alength from)]
    (loop [idx (int starting-index)
           diff (double 0.0)]
      (if (< idx len)
        (let [^double nd (fun (aget from idx))
              d ^double (Math/abs ^double (- ^double (aget to idx) ^double nd))]
          (aset to idx nd)
          (aset from idx 0.0)
          (recur (inc idx)
                 ^double (+ d diff)))
        diff))))

(defn evaluate-neural-net-with-activation-cycles
  [genome inputs activation-cycles & {:keys [threshold]
                                      :or {threshold 0.00001}}]
  (let [output-index (count (filter #(#{:input :bias} (:type %)) (:node-genes genome)))
        outputs (mapv #(dec (:id %)) (filter #(= :output (:type %)) (:node-genes genome)))
        links   (vec (filter :enabled? (:connection-genes genome)))
        preact  (double-array (count (:node-genes genome)) 0.0)
        postact (double-array (count (:node-genes genome)) (into [1.0] inputs))]
    (loop [^long act-cyc activation-cycles
           err (double 99999999.0)]
      (if (or (= act-cyc 0) (< err threshold))
        (mapv #(aget postact %) outputs)
        (do
          (dorun (doseq [l links]
                   (aset preact (dec (:out l)) (+ (aget preact (dec (:out l)))
                                                  (* (:weight l)
                                                     (aget postact (dec (:in l))))))))
          (recur (dec ^long act-cyc)
                 (doubles-map-to-with-diff
                  @transfer-fun
                  preact
                  postact
                  :starting-index output-index)))))))


