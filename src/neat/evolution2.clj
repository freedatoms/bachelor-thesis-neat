(ns neat.evolution2
  (:require [neat
             [population2 :as pop]
             [evolution-parameters :as ep]
             [gui :as gui]
             [utils :as u]]
            [clojure
             [inspector :as ins]]
            [rhizome
             [viz :as viz]])
  (:use [neat
         graphviz-enabled]))

(def file-prefix "/home/frydatom/Dokumenty/FIT/Bakalářka/results/")

(defn- mean 
  [coll]
  (if (>  (count coll) 0)
    (/ (reduce + coll)
       (count coll))
    0.0))

(defn- std [coll]
  (Math/sqrt (float (/ (reduce + (mapv #(Math/pow (- %1 (mean coll)) 2) coll))
                       (count coll)))))

(defn- median
  [coll]
  (nth (sort coll) (Math/floor (float (/ (count coll) 2)))))

(defn- print-stats 
  [coll]
  (let [evals (mapv (partial * @ep/population-size) coll)]
    (prn :mean (mean evals)
         :std (std evals)
         :min (apply min evals)
         :max (apply max evals)
         :median (median evals))))



(defn evolution
  [&{:keys [name do-with-best]
     :or {name "neat",
          do-with-best (fn [most-successful filename] most-successful)}}]
  (reset! ep/innovation-number 0)
  (reset! ep/species-count 0)
  (reset! ep/gene-pool {})
  (let [population (atom (pop/new-population))
        frame (viz/create-frame "NEAT")
        filename (str file-prefix "neat-" name "-gen-%d-succ-%f" " (" (java.util.Date.) ")")]
    (loop [i @ep/generation-count
           max-success -1.0]
      (if (> i 0)
        (let [stats (last (:stats (pop/evolve population)))]
          (printf (str "Generation: %d Species count: %d solved: %d "
                       "Best fitness: %s Avg fitness: %f dt: %f innov: %d success-rate: %f maximal-success-rate: %f\n")
                  (:generation stats)
                  (count (:species stats))
                  (count (:solutions stats))
                  (apply max (mapv :max-fitness (:species stats)))
                  (mean (mapv  :avg-fitness (:species stats)))
                  (:current-dt stats)
                  @ep/innovation-number
                  (float (or (:success-rate stats) -1))
                  max-success)
          (let [most-succ (:most-successful stats)]
            (do-with-best most-succ (format filename (:generation stats) (float (:success-rate most-succ))))
            (u/freeze-to-file (str (format filename (:generation stats) (float (:success-rate most-succ))) ".ind") most-succ)
            (save-image most-succ (str (format filename (:generation stats) (float (:success-rate most-succ))) ".png"))
            (save-dot most-succ (str (format filename (:generation stats) (float (:success-rate most-succ))) ".dot"))
            (view  most-succ frame
                   (format "NEAT generation: %d, #species: %d, fitness: %f, success-rate: %f, maximal-success-rate: %f"
                           (:generation stats)
                           (count (:species stats))
                           (:fitness most-succ)
                           (float (or (:success-rate most-succ) -1))
                           max-success)))
          (recur (dec i) (if (> (:success-rate (:most-successful stats)) max-success)
                           (double (:success-rate (:most-successful stats)))
                           max-success)))
        (:generation @population)))))


