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
         (fn [q] (+ (bit-shift-left q 16) (bit-shift-left q 8) q))
         (normalize-op
          0 255
          (load-file-op a32extent elev))))

(def r2 (run op))

(write-raster-to-file r2 "/Users/ahinz/src/azavea/clojure/file.png")