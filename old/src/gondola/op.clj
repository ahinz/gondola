(ns gondola.op (:gen-class)
    (:require [gondola.core :as core])
    (:use [gondola.core :only (NODATA)])
    (:import gondola.core.Raster))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true) 

(defmacro timeop
  [descr expr]
  `(let [start# (. System (nanoTime))
         blah# (prn "Starting " ~descr)
         ret# ~expr]
     (prn (str ~descr " took: " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " msecs"))
          ret#))

;; (Ctxt -> Raster) -> Raster
(defn run [rasterop]
  "Execute an operation"
  (rasterop nil))

;; Raster -> (Int -> Int) -> Raster
(defn amapi ^ints [^ints a skip-no-data fnc]
  (let [len (alength a)
        newar (int-array len)]
    (loop [idx 0]
      (if (< idx len)
        (do
          (let [item (aget a (unchecked-int idx))]
            (aset-int newar idx
                      (if (and (= NODATA item) skip-no-data)
                        NODATA
                        (unchecked-int (fnc item)))))
          (recur (unchecked-inc idx)))
        newar))))

(defn do-cell-nd [fnc raster]
  "Map over each cell in the given raster with the supplied function"
  (let [data (:data raster)]
    (Raster. (amapi data false fnc)
             (:extent raster)
             (:meta raster))))

(defn do-cell [fnc raster]
  "Map over each cell in the given raster with the supplied function"
  (let [data (:data raster)]
    (Raster. (amapi data true fnc)
             (:extent raster)
             (:meta raster))))

(defn array-min-max-helper [^ints z ^long mini ^long maxi ^long idx]
  (if (< idx (alength z))
    (let
        [a (aget z idx)
         newmin (int (if (and (not= a NODATA) (< a mini)) a mini))
         newmax (int (if (and (not= a NODATA) (> a maxi)) a maxi))]
      (recur z newmin newmax (unchecked-inc idx)))
    [mini maxi]))

(defn array-min-max [^ints z]
  (array-min-max-helper z java.lang.Integer/MAX_VALUE java.lang.Integer/MIN_VALUE 0))

(defn normalize-f [^long smin ^long  smax ^long dmin ^long dmax]
  (fn ^long [^long k]
    (let [pct (double (/ (unchecked-double (- k smin)) (unchecked-double (- smax smin))))]
      (int (+ (* (- dmax dmin) pct) dmin)))))

(defn normalize [dmin dmax raster]
  (let [[smin smax] (array-min-max (:data raster))
        zzz (println "Normalize minmax " smin " " smax)
        normal-f (normalize-f smin smax dmin dmax)]
    (do-cell normal-f raster)))


;;;;;;;;;; Hillshade ;;;;;;;;;;;;;;

(defn deg2rad ^double [^double d]
  (if (> d 360.0)
    (recur (- d 360.0))
    (* d 0.0174532925))) ;; 0.0174532925 --> pi/180

(defmacro ++ [a b c] `(+ (+ ~a ~b) ~c))
(defmacro nthi [v i] `(unchecked-long (nth ~v ~i)))

(defn slope-dz [^long cellsize neigh]
  (let [dzdx (/ (unchecked-double (- (++ (nthi neigh 2) (* 2 (nthi neigh 5)) (nthi neigh 8)) (++ (nthi neigh 0) (* 2 (nthi neigh 3)) (nthi neigh 6)))) (* 8.0 cellsize))
        dzdy (/ (unchecked-double (- (++ (nthi neigh 6) (* 2 (nthi neigh 7)) (nthi neigh 8)) (++ (nthi neigh 0) (* 2 (nthi neigh 1)) (nthi neigh 2)))) (* 8.0 cellsize))]
    [(double dzdx) (double dzdy)]))

(defn slope ^double [^double dzdx ^double dzdy]
  (Math/atan
   (Math/sqrt (+ (* dzdx dzdx)
                 (* dzdy dzdy)))))

(defn aspect ^double [^double dzdx ^double dzdy]
  (if (= 0.0 dzdx)
    (if (> 0.0 dzdy)
      (/ Math/PI 2.0)
      (* 3.0 (/ Math/PI 2.0)))
    (let [aspect_rad (Math/atan2 dzdy (- dzdx))]
      (if (> 0.0 aspect_rad)
        (+ (* Math/PI 2.0) aspect_rad)
        aspect_rad))))

(defn hillshade-cell ^double [^double altitude ^double azimuth ^long cellsize neigh]
  (let [zenith_rad (deg2rad (- 90 altitude))
        azimuth_rad (deg2rad (+ (- 360 azimuth) 90))
        [dzdx dzdy] (slope-dz cellsize neigh)
        slope_rad (slope dzdx dzdy)
        aspect_rad (aspect dzdx dzdy)]
    (* 255.0
       (+ (* (Math/cos zenith_rad) (Math/cos slope_rad))
          (* (* (Math/sin zenith_rad) (Math/sin slope_rad)) (Math/cos (- azimuth_rad aspect_rad)))))))

(defn get-neighbors-or-nodata [^Raster raster ^long col ^long row]
  (let [a (unchecked-int (core/get-cell raster (- col 1) (+ row 1)))
        b (core/get-cell raster col (+ row 1))
        c (core/get-cell raster (+ col 1) (+ row 1))

        d (core/get-cell raster (- col 1) row)
        e (core/get-cell raster col row)
        f (core/get-cell raster (+ col 1) row)

        g (core/get-cell raster (- col 1) (- row 1))
        h (core/get-cell raster col (- row 1))
        i (core/get-cell raster (+ col 1) (- row 1))]
    (if (or
         (= a NODATA)
         (= b NODATA)
         (= c NODATA)
         (= d NODATA)
         (= e NODATA)
         (= f NODATA)
         (= g NODATA)
         (= h NODATA)
         (= i NODATA))
      nil
      [a b c
       d e f
       g h i])))

(defn cr2idx ^long [^long width ^long col ^long row]
  (+ (* width row) col))

(defn do-cell-neighbor [fnc raster]
  (let [data ^ints (:data raster)
        len (alength data)
        width (unchecked-int (core/width (:dimension (:extent raster))))
        newar (int-array len)]
    (do
      (dotimes [row (unchecked-int (core/height (:dimension (:extent raster))))]
        (dotimes [col (unchecked-int (core/width (:dimension (:extent raster))))]
          (let [vec (get-neighbors-or-nodata raster (unchecked-long col) (unchecked-long row))]
            (aset-int newar (cr2idx width col row)
                      (if vec
                        (fnc vec)
                        NODATA)))))
      (Raster. newar (:extent raster) (:meta raster)))))

(defn hillshade [altitude azimuth raster]
  (do-cell-neighbor
   (fn [z]
     (hillshade-cell
      altitude
      azimuth
      (:cellwidth (:extent raster))
      z)), raster))


(defn hillshade-op [altitude azimuth rasterop]
  (fn [z]
    (timeop "hillshade"
            (hillshade altitude azimuth (rasterop z)))))


;;  (Raster, [Int] -> Int), (Ctxt -> Raster) -> (Ctxt -> Raster)
(defn do-cell-neighbor-op [fnc rasterop]
  (fn [z]
    (timeop "do cell neighbor"
            (do-cell-neighbor fnc (rasterop z)))))

;; (Ctxt -> Raster) -> (Int -> Int) -> (Ctxt -> Raster)
(defn do-cell-op [fnc rasterop]
  "Map over each cell in the given raster with the supplied function"
  (fn [z]
    (timeop "do cell" (do-cell fnc (rasterop z)))))

(defn do-cell-nd-op [fnc rasterop]
  "Map over each cell in the given raster with the supplied function"
  (fn [z]
    (timeop "do cell" (do-cell-nd fnc (rasterop z)))))


;; RasterExtent -> String -> (Ctxt -> Raster)
(defn load-file-op [raster-extent file]
  "Load the given arg32 file (basename with no extentions) with the given
   extent"
  (fn [z]
    (timeop "load file" (core/arg32-read raster-extent file))))

(defn normalize-op [dmin dmax rasterop]
  (fn [z]
    (timeop "Normalzie Op" (normalize dmin dmax (rasterop z)))))
