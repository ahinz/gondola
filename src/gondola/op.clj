(ns gondola.op
  (:use gondola.core))


(defn add-literal
  "Add a literal to a raster"
  [raster ^Integer lit]
  (map1 raster (fn [^Integer a] (+ lit a))))


;(run-raster (add-literal gondola.arg/a 9))
