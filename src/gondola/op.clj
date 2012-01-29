(ns gondola.op (:gen-class)
    (:require [gondola.core :as core])
    (:import gondola.core.Raster))

(set! *warn-on-reflection* true)

;; (Ctxt -> Raster) -> Raster
(defn run [rasterop]
  "Execute an operation"
  (rasterop nil))

(def NODATA java.lang.Integer/MIN_VALUE)

(defmacro amapi
  "Maps an expression across an int array a, using an index named idx, and
  return value named ret, initialized to an empty array with same length
  as a, then setting each element of ret to the evaluation of expr,
  returning the new array ret."
  [a idx ret expr]
  `(let [a# ~a
         ~ret (int-array (alength a#))]
     (loop  [~idx 0]
       (if (< ~idx  (alength a#))
         (do
           (aset-int ~ret ~idx ~expr)
           (recur (unchecked-inc ~idx)))
                  ~ret))))

;; Raster -> (Int -> Int) -> Raster
(defn do-cell [fnc raster]
  "Map over each cell in the given raster with the supplied function"
  (let [data (:data raster)]
    (Raster. (amapi ^ints data
                    idx
                    ret
                    (int
                     (let [r (aget ^ints data idx)]
                       (if (= r NODATA)
                         NODATA
                         (fnc (aget ^ints data idx))))))
             (:extent raster)
             (:meta raster))))

;; (Ctxt -> Raster) -> (Int -> Int) -> (Ctxt -> Raster)
(defn do-cell-op [fnc rasterop]
  "Map over each cell in the given raster with the supplied function"
  (fn [z]
    (do-cell fnc (rasterop z))))


;; RasterExtent -> String -> (Ctxt -> Raster)
(defn load-file-op [raster-extent file]
  "Load the given arg32 file (basename with no extentions) with the given
   extent"
  (fn [z]
    (core/arg32-read raster-extent file)))


(defn min-max [[mn mx] a]
  (if (= a NODATA)
    [mn mx]
    [(min mn a) (max mx a)]))

(defn array-min-max [^ints z]
  (areduce z i ret
           [(- NODATA) NODATA]
           (min-max ret (aget z i))))

(defn normalize-f [^long smin ^long  smax ^long dmin ^long dmax]
  (fn ^long [^long k]
    (let [pct (/ (- k smin) (- smax smin))]
      (int (+ (* (- dmax dmin) pct) dmin)))))

(defn normalize [dmin dmax raster]
  (let [[smin smax] (array-min-max (:data raster))
        zzz (println "Normalizing with (" smin ", " smax ") => (" dmin ", " dmax ")")
        normal-f (normalize-f smin smax dmin dmax)]
    (do-cell normal-f raster)))

(defn normalize-op [dmin dmax rasterop]
  (fn [z]
    (normalize dmin dmax (rasterop z))))
