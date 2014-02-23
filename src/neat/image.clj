(ns neat.image
  (:require [incanter 
             [core :as co]])
  (:import javax.swing.ImageIcon
           javax.swing.JFrame
           javax.swing.JLabel
           java.awt.image.BufferedImage
           javax.imageio.ImageIO
           java.io.File))

(set! *warn-on-reflection* true)

(defn image->matrix
  [filename]
  (let [^BufferedImage img (ImageIO/read (File. ^String filename))
        width (.getWidth img)
        height (.getHeight img)
        image (BufferedImage. width height BufferedImage/TYPE_USHORT_GRAY)]
    (.drawImage (.getGraphics image) img 0 0 nil)
    (co/matrix (for [x (range width)
                     y (range height)]
                 [x y (float (/ (.getRGB image x y) 65536))]))))

(defn matrix->image
  [mat]
  (let [maxw (inc (reduce max (co/sel mat :cols 0)))
        maxh (inc (reduce max (co/sel mat :cols 1)))
        image (BufferedImage. maxw maxh BufferedImage/TYPE_USHORT_GRAY)]
    (dorun (doseq [[x y p] (co/to-vect mat)]
             (.setRGB image (int x) (int y) (Math/round ^float (* 65536 p)))))
    image))
(require '[clojure [inspector :as ins]])
(defn show-image
  [img & {:keys [frame title]
          :or {title "Image"}}]
  (let [^JFrame fr (or frame (JFrame.))
        ^ImageIcon imic (ImageIcon. ^BufferedImage img)]
    (if frame
      (.setIcon ^JLabel
                (.getComponent
                 (.getComponent
                  (.getComponent
                   (.getComponent fr 0) 1) 0) 0)
                imic)
      (.add fr^JLabel (JLabel. imic)))
    (.setTitle fr title)
    (when (not (.isShowing fr))
      (.pack fr)
      (.setVisible fr true))))
