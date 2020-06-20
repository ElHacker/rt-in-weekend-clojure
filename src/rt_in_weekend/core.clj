(ns rt-in-weekend.core
  (:require [rt-in-weekend.image :as img]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn pixel-line [r g b]
  (str r " " g " " b "\n"))

(defn create-ppm []
  (let [image_width 256,
        image_height 256,
        header (ppm-header image_width image_height)
        pixels (for [j (range (dec image_height) -1 -1)
                     i (range 0 image_width)
                     :let [r (int (* 255.999 (/ i (dec image_width))))
                           g (int (* 255.999 (/ j (dec image_height))))
                           b (int (* 255.999 0.25))]]
                 (pixel-line r g b))
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm "./images/image")))

(create-ppm)
