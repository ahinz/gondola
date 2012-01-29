(defproject gondola "1.0.0-SNAPSHOT"
  :main gondola.core
  :aot [gondola.core gondola.image gondola.op gondola.example]
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.clojure/math.numeric-tower "0.0.1"]]
  :jvm-opts ["-Xmx1g"] )