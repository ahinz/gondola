(ns gondola.core (:gen-class)
    (:require [clojure.xml :as xml]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true) 

(def NODATA ^int java.lang.Integer/MIN_VALUE)

;;
;;(def xmaxa -8369110.637329101)
;;(def xmina -8370891.749181792)
;;(def ymaxa 4873050.840777581)
;;(def ymina 4868238.558153241)
;;

;; <METADATA>
;; <VERSION value= "1.0"></VERSION>
;; <ID value= "id"></ID>
;; <NAME value= "My new arg32 raster"></NAME>
;; <DESCRIPTION value= "created by geotiff-to-arg32"></DESCRIPTION>
;; <SRID value= "3785"></SRID>
;; <CELLWIDTH value= "30.0"></CELLWIDTH>
;; <CELLHEIGHT value= "30.0"></CELLHEIGHT>
;; <ORIGIN xMin= "-8510221.827729098" yMin= "4844924.626148505"></ORIGIN>
;; <DIMENSIONS width= "5445" height= "5342"></DIMENSIONS>
;; <CREATEDATE value= "Tue Jun 07 17:41:57 EDT 2011"></CREATEDATE>
;; <UPDATEDATE value= "Tue Jun 07 17:41:57 EDT 2011"></UPDATEDATE>
;; </METADATA>

(defrecord Arg32meta [version id name descr srid createdate updatedate geo])

(def x first)
(def y second)
(def width first)
(def height second)

;; Origin is defined as *bottom*left* as 0,0
(defrecord RasterExtent [cellwidth cellheight origin dimension])

;; These all take raster extents
(defn xmin [r] (x (:origin r)))
(defn ymin [r] (y (:origin r)))
(defn xmax [r] (+ (x (:origin r)) (* (width (:dimension r)) (:cellwidth r))))
(defn ymax [r] (+ (y (:origin r)) (* (height (:dimension r)) (:cellheight r))))
(defn bounds [r] (map #(% r) xmin ymin xmax ymax))

(defrecord Raster [data extent meta])

(defn parse-arg32 [file]
  (let [metad (xml/parse file)
        content (:content metad)
        base (apply hash-map (flatten (map (fn [k] (list (:tag k) k)) content)))
        version (:value (:attrs (:VERSION base)))
        id (:value (:attrs (:ID base)))
        name (:value (:attrs (:NAME base)))
        descr (:value (:attrs (:DESCRIPTION base)))
        srid (:value (:attrs (:SRID base)))
        cellw (:value (:attrs (:CELLWIDTH base)))
        cellh (:value (:attrs (:CELLHEIGHT base)))
        origin (:attrs (:ORIGIN base))
        origin-x (:xMin origin)
        origin-y (:yMin origin)      
        dims (:attrs (:DIMENSIONS base))
        width (:width dims)
        height (:height dims)
        create (:value (:attrs (:CREATEDATE base)))
        update (:value (:attrs (:UPDATEDATE base)))]
   (Arg32meta. version id name descr srid create update
               (RasterExtent.
                (Double/parseDouble cellw) (Double/parseDouble cellh)
                (list
                 (Double/parseDouble origin-x)
                 (Double/parseDouble origin-y))
                (list
                 (Integer/parseInt width)
                 (Integer/parseInt height))))))

(defn get-mmap-buffer ^java.nio.MappedByteBuffer [^String file len]
  (.map (.getChannel (java.io.FileInputStream. file)) java.nio.channels.FileChannel$MapMode/READ_ONLY 0 len))

(defn raster-ncells [raster]
  (let [extent (:extent raster)
        dim (:dimension extent)]
    (* (first dim) (second dim))))

(defn get-cell ^long [^Raster raster ^long c ^long r]
  (let [w (width (:dimension (:extent raster)))
        h (height (:dimension (:extent raster)))
        len (alength ^ints (:data raster))
        idx (int (+ (* r w) c))]
    (if (and
         (>= c 0)
         (>= r 0)
         (< c w)
         (< r h)
         (>= idx 0)
         (< idx len))
      (aget ^ints (:data raster) idx)
      NODATA)))

;; Read info from [base] into the given raster extent
(defn arg32-read [raster-extent base]
  (let [xmlf (str base ".xml"),
        rawf (str base ".arg32")
        a32md (parse-arg32 xmlf)
        srcRasterMeta (:geo a32md)
        srcDim (:dimension srcRasterMeta)
        srcCellHeight (int (:cellheight srcRasterMeta))
        srcCellWidth (int (:cellwidth srcRasterMeta))
        srcNCells (int (* (width srcDim) (height srcDim)))

        zzz (println "Loading cells *4: " srcNCells)
        
        srcBuffer (.asIntBuffer (get-mmap-buffer rawf (* 4 srcNCells)))
        
        srcXmin (double (xmin srcRasterMeta))
        srcYmin (double (ymin srcRasterMeta))
        srcXmax (double (xmax srcRasterMeta))
        srcYmax (double (ymax srcRasterMeta))

        srcWidth (double (- srcXmax srcXmin))
        srcHeight (double (- srcYmax srcYmin))
        
        srcRows (int (/ srcHeight srcCellHeight))
        srcCols (int (/ srcWidth srcCellWidth))

        destXmin (double (xmin raster-extent))
        destYmin (double (ymin raster-extent))
        destXmax (double (xmax raster-extent))
        destYmax (double (ymax raster-extent))

        destCellWidth (double (:cellwidth raster-extent))
        destCellHeight (double (:cellheight raster-extent))

        destWidth (double (- destXmax destXmin))
        destHeight (double (- destYmax destYmin))

        destRows (int (/ (double destHeight) destCellHeight))
        destCols (int (/ (double destWidth) destCellWidth))

        destNCells (int (* destRows destCols))

        ;; x/yBase is where src is at dest cell (0,0)
        ;; i.e. Dest(0,0) = Src(xBase,yBase)
        xBase (double (+ (- destXmin srcXmin) (/ destCellWidth 2)))
        yBase (double (+ (- destYmin srcYmin) (/ destCellHeight 2)))

        zzz (println "Width: " destWidth " height " destHeight " rows: " destRows " cols: " destCols)
        
        destArray (int-array destNCells NODATA)

        minCol (int (/ xBase srcCellWidth))
        maxCol (int (/ (+ xBase (* destCols destCellWidth)) srcCellWidth))]

    ;; Loop over rows    
    (loop [destRow 0 y yBase]
      (if (>= destRow destRows)
        (Raster. destArray raster-extent nil)
          
        ;; Current source row given height
        (let [srcRow (- srcRows (unchecked-int (/ y srcCellHeight)))
              srcSpan (* srcRow srcCols)
              destSpan (* destCols (- (- destRows 1) destRow))]
            
          ;; skip calc if we aren't in the source at all
          (do
            (if (and
                 (< (+ srcSpan minCol) srcNCells)
                 (>= (+ srcSpan maxCol) 0))
              (loop [destCol 0 x xBase]
                (if (>= destCol destCols)
                  nil ;; Ignore return value due to mutating array
                  (let [srcCol (unchecked-divide-int (unchecked-int x) srcCellWidth)
                        srcIndex (+ (unchecked-int srcSpan) srcCol)
                        destIndex (+ destSpan destCol)]
                    (do
                      (if (and
                           (>= srcCol 0)
                           (< srcCol srcCols)
                           (< srcIndex srcNCells)
                           (>= srcIndex 0))
                        (java.lang.reflect.Array/setInt destArray (unchecked-int destIndex) (.get srcBuffer (unchecked-int srcIndex)))
                        nil)
                      (recur (+ destCol 1) (+ x destCellWidth)))))))   
            (recur (+ 1 destRow) (+ destCellHeight y))))))))

(defn latlon2webmercator [lat lon]
  (let [a (* lat 0.017453292519943295)]
    (list
     (* lon 0.017453292519943295 6378137.0)
     (* 3189068.5
        (java.lang.Math/log (/ (+ 1.0 (java.lang.Math/sin a))
                               (- 1.0 (java.lang.Math/sin a))))))))


