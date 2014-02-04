(ns neat.individual
  (:require [neat
             [genome :as genome]
             [species :as species]]
            [rhizome
             [viz :as viz]])
  (:use [neat
         evolution-parameters
         graphviz-enabled]))

(defrecord Individual
    [genome
     raw-fitness
     fitness]
  GraphvizEnabled
  (save-image [this filename]
    (save-image (:genome this) filename))
  (save-dot [this filename]
    (save-dot (:genome this) filename))
  (view [this]
    (view (:genome this) (viz/create-frame (format "Individual raw-fitness=%f fitness=%f"
                                                   (:raw-fitness this)
                                                   (:fitness this)))))
  (view [this frame]
    (view (:genome this) frame))
  (view [this frame title]
    (view (:genome this) frame title)))

(defn new-individual
  [& {:keys [genome inputs outputs]}]
  {:pre [(or (and inputs outputs) genome)]}
  (let [gen (or genome (genome/initial-genome inputs outputs))
        raw-fit (@fitness-fun gen)
        fit     (species/fitness-share gen raw-fit)]
    (->Individual gen raw-fit fit)))
