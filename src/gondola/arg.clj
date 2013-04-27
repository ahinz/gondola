(ns gondola.arg
  (:use gondola.core)
  (:require [clojure.data.json :as json]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn- byte-memory-mapped-raster-map1 [r f]
  (let [buffer ^java.nio.MappedByteBuffer (:buffer r)
        size (:size r)
        a (byte-array size)]
    (loop [i 0]
      (aset-byte a i (f (.get buffer)))
      (if (< i size)
        (recur (+ i 1))))
    (create-byte-array-backed-raster (:extent r) a)))

(defn- bb-skip [^java.nio.ByteBuffer b ^Integer amt]
  (.position b (+ (.position b) amt)))

(defn- byte-memory-mapped-raster-crop
  [orig-extent xmin ymin xmax ymax]
  (fn [r f]
    (let [buffer ^java.nio.MappedByteBuffer (:buffer r)
          rows (:rows orig-extent)
          cols (:rows orig-extent)
          a (byte-array (* (- xmax xmin) (- ymax ymin)))]
      ; Skip ymin rows
      (bb-skip buffer (* ymin cols))
      (loop [row ymin]
        ; Skip starting cols
        (bb-skip buffer xmin)
        (loop [col xmin]
          (aset-byte a (+ (* (- row ymin) (- xmax xmin)) (- col xmin)) (f (.get buffer)))
          (if (< (+ col 1) xmax)
            (recur (+ col 1))))
        ; Skip end cols
        (bb-skip buffer (- cols xmax))
        (if (< (+ row 1) ymax)
          (recur (+ row 1))))
      (create-byte-array-backed-raster (:extent r) a))))

(defrecord ByteMemoryMappedRaster
    [extent data-type size ^java.nio.MappedByteBuffer buffer]
  Raster
  (map1 [this f]
    (create-mapped-raster extent data-type this f byte-memory-mapped-raster-map1))
  (crop [this colmin rowmin colmax rowmax]
    (let [e (crop-extent extent colmin rowmin colmax rowmax)]
      (create-mapped-raster
       e
       data-type
       (ByteMemoryMappedRaster. e (:data-type this) (:size this) (:buffer this)) identity
       (byte-memory-mapped-raster-crop extent colmin rowmin colmax rowmax)))))

(defn bits-for-datatype
  "Get the number of bits in a given datatype"
  [data-type]
  (condp = data-type
      "int8" 8
      (throw (Exception. (format "Invalid datatype: %s" data-type)))))

(defn create-file-backed-raster [extent data-type ^String file-name]
  (let [bits-per-cell (bits-for-datatype data-type)
        length-in-bytes (/ (* bits-per-cell (:rows extent) (:cols extent)) 8)]
    (condp = data-type
      "int8"
      (ByteMemoryMappedRaster.
       extent
       data-type
       (* (:rows extent) (:cols extent))
       (.map (.getChannel (java.io.FileInputStream. file-name))
             java.nio.channels.FileChannel$MapMode/READ_ONLY 0 length-in-bytes))

      (throw (Exception. (format "Invalid datatype: %s" data-type))))))


(defn load-arg
  "Load an 'Azavea Raster Grid' file. argfile should
   be a path to a file ending in .arg"
  [^String argfile]
  (let [jsonfile (.replaceAll argfile "(.*)\\.arg$" "$1.json")
        metadata (json/read-str (slurp jsonfile))
        data-type (get metadata "datatype")
        bbox (apply bbox-create (map (partial get metadata)
                                     ["xmin" "ymin" "xmax" "ymax"]))
        re (apply extent-create bbox (map (partial get metadata)
                                          ["cellwidth" "cellheight" "rows" "cols"]))]
    (create-file-backed-raster re data-type argfile)))

;(load-arg "/Users/ahinz/src3/gondola/Canals_percNHD-huc12.arg")
