(ns gondola.rest (:gen-class)
    (:use compojure.core gondola.example)
    (:require
     [gondola.core :as raster]
     [compojure.route :as route]
     [compojure.handler :as handler]))

(defmacro s2d [s] `(Double/parseDouble ~s))
(defmacro s2i [s] `(Integer/parseInt ~s))

(defroutes main-routes
  (GET "/image" [xmin ymin xmax ymax width height]
       {
        :status 200
        :body (java.io.ByteArrayInputStream. (.toByteArray (hs (s2d xmin) (s2d ymin) (s2d xmax) (s2d ymax) (s2i width) (s2i height))))
        :headers {"Content-Type" "image/png"}})
  
  (GET "/image2" [] {:status 200
                     :body (java.io.ByteArrayInputStream. (.toByteArray (woz)))
                     :headers {"Content-Type" "image/png"}
                     })
  (GET "/" [] "<h1>Hello World Wide Web!</h1>")
  (route/resources "/")
  (route/not-found "Page not found"))

(def app
    (handler/site main-routes))