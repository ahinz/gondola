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
         (fn [^long q] (if (or (= q NODATA) (< 0 q)) 0 (unchecked-int (+ 0xff000000 (+ (+ (* q 65536) (* q 256)) q)))))
         (hillshade-op
          0 315
          (load-file-op raster-extent elev))))

(defn cell2clr [^long q]
  (if (or (= q NODATA) (< 0 q))
    0x00000000
    (let [qpp (bit-and q 0xff)
          qp 0]
      (unchecked-int (+ 0xff000000 (+ (+ (* qp 65536) (* qp 256)) qp))))))

(defn hs [xmin ymin xmax ymax width height]
  (let [cw (/ (- xmax xmin) width)
        ch (/ (- ymax ymin) height)
        extent (RasterExtent. cw ch [xmin ymin] [width height])
        op (do-cell-nd-op
            cell2clr
            (hillshade-op
             0 315
             (load-file-op extent elev)))]
    (write-raster-to-byte-array-output-stream (run op))))


(defn woz [] (write-raster-to-byte-array-output-stream (run op)))
(defn wop [] (write-raster-to-file (run op) "/Users/ahinz/src/azavea/clojure/file.png"))
(defn -main [] (write-raster-to-file (run op) "/Users/ahinz/src/azavea/clojure/file.png"))
