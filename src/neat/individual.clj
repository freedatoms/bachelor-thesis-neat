(ns neat.individual
  (:require [neat
             [genome :as genome]]
            [rhizome
             [viz :as viz]])
  (:use [neat
         evolution-parameters
         graphviz-enabled]))

(defrecord Individual
    [genome
     raw-fitness
     fitness
     expected-offspring
     successful]
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
        [raw-fit success?] (@fitness-fun gen)]
    (->Individual gen raw-fit 0 0 success?)))
