(ns rt-in-weekend.core
  (:require [rt-in-weekend.image :as img]
            [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]
            [rt-in-weekend.hittable :as hittable]
            [rt-in-weekend.camera :as camera])
  (:import (java.util Random)))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn pixel-line [r g b]
  (str r " " g " " b "\n"))

(defn drand-48 []
  (.nextFloat (Random.)))

(defn raytrace [width height pixels path]
  (let [header (ppm-header width height)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm path)))

(defn ray-color [r world]
  (if-let [rec (hittable/hittable-list world r 0.0 Float/MAX_VALUE)]
    (do
      (vec/* (map inc (:normal rec)) 0.5))
    (let [unit-direction (vec/unit-vector (ray/direction r))
          t (* 0.5 (inc (vec/y unit-direction)))]
      (vec/+ (vec/* [1.0 1.0 1.0] (- 1.0 t))
             (vec/* [0.5 0.7 1.0] t)))))

(defn evolve-color [world cam image-width image-height num-samples i j]
  (let [color (atom [0 0 0])]
    (doseq [s (range num-samples)]
      (let [u (/ (+ i (drand-48)) (float image-width))
            v (/ (+ j (drand-48)) (float image-height))
            r (camera/get-ray cam u v)]
        (swap! color vec/+ (ray-color r world))))
    (vec// @color (float num-samples))))

(defn simple-background-and-sphere []
  (let [aspect_ratio (/ 16.0 9.0)
        image-width 384
        image-height (int (/ image-width aspect_ratio))
        num-samples 100
        viewport_height 2.0
        viewport_width (* aspect_ratio viewport_height)
        focal_length 1.0
        horizontal [viewport_width 0.0 0.0]
        vertical [0.0 viewport_height 0.0]
        origin [0.0 0.0 0.0]
        lower-left-corner (vec/- (vec/- (vec/- origin (vec// horizontal 2))
                                        (vec// vertical 2))
                                 [0.0 0.0 focal_length])
        world [(hittable/->Sphere [0 0 -1] 0.5)
               (hittable/->Sphere [0 -100.5 -1] 100)]
        cam(camera/make lower-left-corner horizontal vertical origin)]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam image-width image-height num-samples i j)
                          ir (int (* 255.999 (vec/x color)))
                          ig (int (* 255.999 (vec/y color)))
                          ib (int (* 255.999 (vec/z color)))]]
                (pixel-line ir ig ib))
              "./images/background-sphere-surface-antialias")))

(defn create-ppm []
  (let [image-width 256,
        image-height 256,
        header (ppm-header image-width image-height)
        pixels (for [j (range (dec image-height) -1 -1)
                     i (range 0 image-width)
                     :let [r (int (* 255.999 (/ i (dec image-width))))
                           g (int (* 255.999 (/ j (dec image-height))))
                           b (int (* 255.999 0.25))]]
                 (pixel-line r g b))
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm "./images/image")))

(time (simple-background-and-sphere))
