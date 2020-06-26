(ns rt-in-weekend.core
  (:require [rt-in-weekend.image :as img]
            [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]
            [rt-in-weekend.hittable :as hittable]
            [rt-in-weekend.camera :as camera]
            [rt-in-weekend.material :as material]))

(defn ppm-header [width height]
  (str "P3\n" width " " height "\n255\n"))

(defn pixel-line [r g b]
  (str r " " g " " b "\n"))

(defn raytrace [width height pixels path]
  (let [header (ppm-header width height)
        body (clojure.string/join pixels)
        ppm (str header body)]
    (img/save-ppm ppm path)))

(defn ray-color [r world depth]
  (if-let [rec (hittable/hittable-list world r 0.0001 Float/MAX_VALUE)]
    (let [result (material/scatter (:material rec) r rec)]
      (if (and (> depth 0) (:ok result))
        (vec/* (:attenuation result) (ray-color (:scattered result) world (dec depth)))
        [0 0 0]))
    (let [unit-direction (vec/unit-vector (ray/direction r))
          t (* 0.5 (inc (vec/y unit-direction)))]
      (vec/+ (vec/* [1.0 1.0 1.0] (- 1.0 t))
             (vec/* [0.5 0.7 1.0] t)))))

(defn evolve-color [world cam image-width image-height num-samples i j depth]
  (let [color (atom [0 0 0])]
    (doseq [_ (range num-samples)]
      (let [u (/ (+ i (rand)) (float image-width))
            v (/ (+ j (rand)) (float image-height))
            r (camera/get-ray cam u v)]
        (swap! color vec/+ (ray-color r world depth))))
    (vec// @color (float num-samples))))

(defn simple-background-and-sphere []
  (let [aspect_ratio (/ 16.0 9.0)
        image-width 384
        image-height (int (/ image-width aspect_ratio))
        num-samples 30
        max-depth 50
        viewport_height 2.0
        viewport_width (* aspect_ratio viewport_height)
        focal_length 1.0
        horizontal [viewport_width 0.0 0.0]
        vertical [0.0 viewport_height 0.0]
        origin [0.0 0.0 0.0]
        lower-left-corner (vec/- (vec/- (vec/- origin (vec// horizontal 2))
                                        (vec// vertical 2))
                                 [0.0 0.0 focal_length])
        world [(hittable/->Sphere [0 0 -1] 0.5 (material/->Lambertian [0.1 0.2 0.5]))
               (hittable/->Sphere [0 -100.5 -1] 100 (material/->Lambertian [0.8 0.8 0.0]))
               (hittable/->Sphere [1 0 -1] 0.5 (material/->Metal [0.8 0.6 0.2] 0.0))
               (hittable/->Sphere [-1 0 -1] 0.5 (material/->Dialectric 1.5))]
        cam(camera/make lower-left-corner horizontal vertical origin)]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/background-sphere-glass-refract")))

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
