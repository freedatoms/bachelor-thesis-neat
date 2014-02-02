(ns neat.graphviz-enabled
  (:import [javax.swing JFrame ImageIcon]))

(def ^:dynamic *show-edge-label* false)
(def ^:dynamic *show-node-label* true)
(def ^:dynamic *graphviz-dpi* 96)

(defprotocol GraphvizEnabled
  (save-image [this filename] "Saves the image of this")
  (save-dot [this filename] "Saves the dot source for this")
  (view [this] [this frame] [this frame title] "Shows this in frame with title"))
