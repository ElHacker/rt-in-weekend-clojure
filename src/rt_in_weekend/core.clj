(ns rt-in-weekend.core
  (:require [rt-in-weekend.image :as img]
            [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn pixel-line [r g b]
  (str r " " g " " b "\n"))

(defn raytrace [width height pixels path]
  (let [header (ppm-header width height)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm path)))

(defn ray-color [r]
  (if (ray/hit-sphere [0 0 -1] 0.5 r)
    [1 0 0] ; draw red color
    (let [unit-direction (vec/unit-vector (ray/direction r))
          t (* 0.5 (+ (vec/y unit-direction) 1.0))]
      (vec/+ (vec/* [1.0 1.0 1.0] (- 1.0 t))
             (vec/* [0.5 0.7 1.0] t)))))

(defn simple-background-and-sphere []
  (let [aspect_ratio (/ 16.0 9.0)
        image_width 384
        image_height (int (/ image_width aspect_ratio))
        viewport_height 2.0
        viewport_width (* aspect_ratio viewport_height)
        focal_length 1.0
        horizontal [viewport_width 0.0 0.0]
        vertical [0.0 viewport_height 0.0]
        origin [0.0 0.0 0.0]
        lower-left-corner (vec/- (vec/- (vec/- origin (vec// horizontal 2))
                                        (vec// vertical 2))
                                 [0.0 0.0 focal_length])]
    (raytrace image_width image_height
              (for [j (range (dec image_height) -1 -1)
                    i (range 0 image_width)
                    :let [u (/ (double i) (dec image_width))
                          v (/ (double j) (dec image_height))
                          r (ray/make origin (vec/- (vec/+ lower-left-corner
                                                           (vec/+ (vec/* horizontal u)
                                                                  (vec/* vertical v)))
                                                    origin))
                          color (ray-color r)
                          ir (int (* 255.999 (vec/x color)))
                          ig (int (* 255.999 (vec/y color)))
                          ib (int (* 255.999 (vec/z color)))]]
                (pixel-line ir ig ib))
              "./images/background-sphere")))

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

(comment (create-ppm))
(simple-background-and-sphere)
