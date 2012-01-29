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
(defn amapi ^ints [^ints a fnc]
  (let [len (alength a)
        newar (int-array len)]
    (loop [idx 0]
      (if (< idx len)
        (do
          (let [item (aget a (unchecked-int idx))]
            (aset-int newar idx
                      (if (= NODATA item)
                        NODATA
                        (fnc item))))
          (recur (unchecked-inc idx)))
        newar))))

(defn do-cell [fnc raster]
  "Map over each cell in the given raster with the supplied function"
  (let [data (:data raster)]
    (Raster. (amapi data fnc)
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

(defn deg2rad [d] (if (> d 360)
                    (recur (- d 360))
                    (/ (* d Math/PI) 180.0)))

(defmacro ++ [a b c] `(+ (+ ~a ~b) ~c))

(defn slope-dz [cellsize [a b c
                       d e f
                       g h i]]
  (let [dzdx (/ (- (++ c (* 2 f) i) (++ a (* 2 d) g)) (* 8 cellsize))
        dzdy (/ (- (++ g (* 2 h) i) (++ a (* 2 b) c)) (* 8 cellsize))]
    [(double dzdx) (double dzdy)]))

(defn slope [dzdx dzdy]
  (Math/atan
   (Math/sqrt (+ (* dzdx dzdx)
                 (* dzdy dzdy)))))

(defn aspect [dzdx dzdy]
  (if (= 0 dzdx)
    (if (> 0 dzdy)
      (/ Math/PI 2)
      (* 3 (/ Math/PI 2)))
    (let [aspect_rad (Math/atan2 dzdy (- dzdx))]
      (if (> 0 aspect_rad)
        (+ (* Math/PI 2.0) aspect_rad)
        aspect_rad))))

(defn hillshade-cell [altitude azimuth cellsize neigh]
  (let [zenith_rad (deg2rad (- 90 altitude))
        azimuth_rad (deg2rad (+ (- 360 azimuth) 90))
        [dzdx dzdy] (slope-dz cellsize neigh)
        slope_rad (slope dzdx dzdy)
        aspect_rad (aspect dzdx dzdy)]
    (* 255.0
       (+ (* (Math/cos zenith_rad) (Math/cos slope_rad))
          (* (* (Math/sin zenith_rad) (Math/sin slope_rad)) (Math/cos (- azimuth_rad aspect_rad)))))))

(defn get-neighbors-or-nodata [raster col row]
  (let [a (core/get-cell raster (- col 1) (+ row 1))
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
      NODATA
      [a b c
       d e f
       g h i])))

(defn do-cell-neighbor [fnc raster]
  (let [data ^ints (:data raster)
        len (alength data)
        width (core/width (:dimension (:extent raster)))
        newar (int-array len)]
    (do
      (dotimes [row (core/height (:dimension (:extent raster)))]
        (dotimes [col (core/width (:dimension (:extent raster)))]
          (let [vec (get-neighbors-or-nodata raster col row)]
            (aset-int newar (+ (* row width) col)
                      (if (= NODATA vec)
                        NODATA
                        (fnc vec))))))
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


;; RasterExtent -> String -> (Ctxt -> Raster)
(defn load-file-op [raster-extent file]
  "Load the given arg32 file (basename with no extentions) with the given
   extent"
  (fn [z]
    (timeop "load file" (core/arg32-read raster-extent file))))

(defn normalize-op [dmin dmax rasterop]
  (fn [z]
    (timeop "Normalzie Op" (normalize dmin dmax (rasterop z)))))
