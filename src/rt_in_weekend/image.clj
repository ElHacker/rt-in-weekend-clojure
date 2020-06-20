(ns rt-in-weekend.image
  (:require [clojure.java.io :as io])
  (:import javax.imageio.ImageIO
           (java.awt.image BufferedImage)))

(defn save-ppm [^String ppm ^String path]
  (println "Saving PPM: " path)
  (spit path ppm)
  (println "Done Saving PPM: " path))
