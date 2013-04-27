(ns gondola.core)

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defrecord BBox [xmin ymin xmax ymax])
(defrecord RasterExtent [bbox cellwidth cellheight rows cols])
(defrecord ArrayBackedRaster [extent data])
(defrecord ByteArrayBackedRaster [extent data])

(defn create-byte-array-backed-raster [extent data]
  (ByteArrayBackedRaster. extent data))

(defn bbox-create [xmin ymin xmax ymax]
  (BBox. (min xmin xmax) (min ymin ymax)
         (max xmin xmax) (max ymin ymax)))

(defn extent-create [bbox cw ch rows cols]
  (RasterExtent. bbox cw ch rows cols))

(def NODATA ^int java.lang.Integer/MIN_VALUE)

(defprotocol Raster
  "Primitives for dealing with rasters"
  (map1 [this f] "Map each cell with the given function")
  (crop [this colmin rowmin colmax rowmax] "Return a new raster cropped to the given window")
  (run-raster [this] "Return a promise with the raster evaluated"))


(defrecord BBox [xmin ymin xmax ymax])
(defrecord RasterExtent [bbox cellwidth cellheight rows cols])

(defn crop-extent [extent colmin rowmin colmax rowmax]
  (let [cw (:cellwidth extent)
        ch (:cellheight extent)
        new-xmin (+ (:xmin (:bbox extent)) (* colmin cw))
        new-ymin (+ (:ymin (:bbox extent)) (* rowmin ch))]
    (extent-create
     (bbox-create new-xmin new-ymin
                  (+ new-xmin (* (- colmax colmin) cw))
                  (+ new-ymin (* (- rowmax rowmin) ch)))
     cw ch
     (- rowmax rowmin)
     (- colmax colmin))))

(defrecord MappedRaster [extent data-type base-raster f exec]
  Raster
  (map1 [this f2]
    (MappedRaster. extent data-type base-raster (comp f f2) exec))
  (crop [this colmin rowmin colmax rowmax]
    (let [cropped (crop base-raster colmin rowmin colmax rowmax)
          new-extent (:extent cropped)]
      (MappedRaster.
       new-extent data-type cropped f exec)))
  (run-raster [this]
    (exec base-raster f)))

(defn create-mapped-raster [extent data-type base-raster f exec]
  (MappedRaster. extent data-type base-raster f exec))


;(run-raster (crop (add-literal (gondola.arg/load-arg "/Users/ahinz/src3/gondola/Canals_percNHD-huc12.arg") 9) 40 100 20 200))
