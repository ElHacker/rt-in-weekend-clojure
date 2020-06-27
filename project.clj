(defproject rt-in-weekend "0.1.0-SNAPSHOT"
  :description "Implementing Ray Tracing in One Weekend with Clojure"
  :url "https://raytracing.github.io/books/RayTracingInOneWeekend.html"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot rt-in-weekend.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :repl-options {:timeout 240000})
