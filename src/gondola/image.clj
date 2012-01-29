(ns gondola.image (:gen-class)
    (:require [gondola.core :as core]))

(set! *warn-on-reflection* true)

(defn create-image [w h]
  (java.awt.image.BufferedImage. w h java.awt.image.BufferedImage/TYPE_INT_RGB))

(defn write-buffer [^java.awt.image.BufferedImage img buffer]
  (do
    (let [width (.getWidth img)
          height (.getHeight img)]
      (.setDataElements (.getRaster img) 0 0 width height buffer))
    img))

(defn write-to-file [^java.awt.image.BufferedImage image ^String file]
  (javax.imageio.ImageIO/write image "png" (java.io.File. file)))

(defn write-raster-to-file [raster file]
  (let [dim (:dimension (:extent raster))]
    (write-to-file
     (write-buffer (create-image (core/width dim) (core/height dim)) (:data raster))
     file)))

