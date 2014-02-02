(ns neat.genome
  (:require [neat
             [gene :as gene]
             [evolution-parameters :as ep]]
            [rhizome
             [viz :as viz]
             [dot :as dot]])
  (:use [neat graphviz-enabled]
        [clojure.string :only (join)])
  (:import [javax.swing
            JFrame
            ImageIcon]))

(declare -do-graphviz-fun)

(defrecord Genome
    [node-genes
     connection-genes]
  GraphvizEnabled
  (save-image [this filename]
    )
  (save-dot [this filename]
    )
  (view [this]
    (-do-graphviz-fun viz/view-graph this))
  (view [this frame]
    (let [[^JFrame frame ^ImageIcon image-icon] @frame]
      (.setTitle frame "Genome")
      (.setImage image-icon (-do-graphviz-fun viz/graph->image this))
      (if (.isShowing frame)
        (.repaint frame)
        (.setVisible frame true))))
  (view [this frame title]
    (let [[^JFrame frame ^ImageIcon image-icon] @frame]
      (.setTitle frame title)
      (.setImage image-icon (-do-graphviz-fun viz/graph->image this))
      (if (.isShowing frame)
        (.repaint frame)
        (.setVisible frame true)))))

(defn- genome->graph
  [^Genome gen]
  (loop [cg (:connection-genes gen)
         res {:node-gene [:conn-gene]}]
    (if cg
      (recur (next cg) (update-in res [(:in (first cg))] conj (:out (first cg))))
      res)))

(defn- node-genes->records
  [genes]
  (mapv #(vector (:id %) (case (:type %)
                                     :input "input"
                                     :bias "bias"
                                     :hidden "hidden"
                                     :output "output")) genes))

(defn- conn-genes->records
  [genes]
  (mapv #(vector (:innov %)
                 (format "%s -&gt; %s" (:in %) (:out %))
                 (if (:enabled? %)
                   "enabled"
                   "disabled")
                 (:weight %)) genes))

(defn- -do-graphviz-fun
  [fun g]
  (fun (into (mapv :id (:node-genes g)) [:conn-gene :node-gene]) (genome->graph g)
       :node->descriptor (fn [x]
                           (case x
                             :node-gene {:shape "record"
                                         :label (node-genes->records (:node-genes g))
                                         :rank "9999"}
                             :conn-gene {:shape "record"
                                         :label (conn-genes->records (:connection-genes g))
                                         :rank "5"}
                             {:label (str x),
                              :shape "circle",
                              :style "filled",
                              :fillcolor (case (:type (nth (:node-genes g) (dec x)))
                                           :input "#55ff55"
                                           :bias "#aaffff"
                                           :hidden "gray"
                                           :output "#ff5555"
                                           "white")}))
       :edge->descriptor (fn [in _]
                           (case in
                             :node-gene {:style "invis"}
                             {}))
       :node->cluster (into {:node-gene :gene,
                             :conn-gene :gene}
                            (mapv #(vector (:id %) (case (:type %)
                                                     :input :input
                                                     :bias  :input
                                                     :output :output
                                                     nil)) (:node-genes g)))
       :cluster->descriptor (fn [cluster]
                              (case cluster
                                :gene {:style "invis"}
                                {:style "invis"}))
       :options {:splines "polyline"}))


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
                           (gene/->Connection-gene (:id in) (:id out) 1 true 
                                                   (swap! innov inc))
                           ))]
    (->Genome nodes connections)))

(defn match-genes
  [^Genome g1 ^Genome g2]
  (loop [i1 (first (:connection-genes g1))
         i2 (first (:connection-genes g2))
         r1 (next (:connection-genes g1))
         r2 (next (:connection-genes g2))
         res []]
    (if (or i1 i2)
      (cond
       (or (not (and i1 i2)) (= (:innov i1) (:innov i2))) (recur (first r1)
                                                                 (first r2)
                                                                 (next r1)
                                                                 (next r2)
                                                                 (conj res [i1 i2]))
       (> (:innov i1) (:innov i2))  (recur i1
                                           (first r2)
                                           r1
                                           (next r2)
                                           (conj res [nil i2]))
       (< (:innov i1) (:innov i2))  (recur (first r1)
                                           i2
                                           (next r1)
                                           r2
                                           (conj res [i1 nil])))
      res)))

(defn- excess
  [^Genome g1 ^Genome g2]
  (let [minG (min (:innov (last (:connection-genes g1)))
               (:innov (last (:connection-genes g2))))
        matchG (match-genes g1 g2)]
    (mapv (fn [[x y]] (or x y)) (filter (fn [[x y]] (> (:innov (or x y)) minG)) matchG))))

(defn- excess-count
  [^Genome g1 ^Genome g2]
  (count (excess g1 g2)))

(defn- disjoint
  [^Genome g1 ^Genome g2]
  (let [minG (min (:innov (last (:connection-genes g1)))
               (:innov (last (:connection-genes g2))))
        matchG (match-genes g1 g2)]
    (mapv (fn [[x y]] (or x y)) (filter (fn [[x y]] (and (not (and x y))
                                                       (<= (:innov (or x y)) minG)))
                                       matchG))))

(defn- disjoint-count
  [^Genome g1 ^Genome g2]
  (count (disjoint g1 g2)))

(defn- weight-diff
  [^Genome g1 ^Genome g2]
  (let [ab-set (clojure.set/intersection (set (mapv :innov (:connection-genes g1)))
                                         (set (mapv :innov (:connection-genes g2))))
        a (vec (filter #(ab-set (:innov %)) (:connection-genes g1)))
        b (vec (filter #(ab-set (:innov %)) (:connection-genes g2)))]
    (/ (reduce + (map #(Math/abs (- (:weight %1) (:weight %2))) a b))
       (count a))))

(defn delta
  [^Genome g1 ^Genome g2]
  (+ (/ (+ (* (excess-count g1 g2) @ep/c1)
           (* (disjoint-count g1 g2) @ep/c2))
        (max (count (:connection-genes g1))
             (count (:connection-genes g2))))
     (* (weight-diff g1 g2) @ep/c3)))




