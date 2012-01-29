(ns gondola.example (:gen-class)
    (:require gondola.core)
    (:use gondola.core gondola.op gondola.image)
    (:import gondola.core.Raster gondola.core.RasterExtent))

(set! *warn-on-reflection* true)

(def xmaxa -8414862.948045136)
(def xmina -8419538.366658453)
(def ymaxa 4909195.839769209)
(def ymina 4904380.530930891)

(def raster-extent (RasterExtent. 30 30 (list xmina ymina) (list (int (/ (- xmaxa xmina) 30)) (int (/ (- ymaxa ymina) 30)))))
(def elev "/Users/ahinz/src/azavea/data/elevation30m-20110607")
(def a32extent (:geo (parse-arg32 (str elev ".xml"))))

(def op (do-cell-op
         (fn [^long q] (if (or (= q NODATA) (< 0 q)) 0 (int (+ (+ (* q 65536) (* q 256)) q))))
         (hillshade-op
          0 315
          (load-file-op raster-extent elev))))


(defn wop [] (write-raster-to-file (run op) "/Users/ahinz/src/azavea/clojure/file.png"))
(defn -main [] (write-raster-to-file (run op) "/Users/ahinz/src/azavea/clojure/file.png"))

(wop)