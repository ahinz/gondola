(defproject gondola "1.0.0-SNAPSHOT"
  :main gondola.example
  :aot [gondola.core gondola.image gondola.op gondola.example gondola.rest]
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]
                 [compojure "0.6.4"]]
  :dev-dependencies [[lein-ring "0.4.5"]]
  :ring {:handler gondola.rest/app}
  
  :jvm-opts ["-Xmx1g"])