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
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 384
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 50
        lookfrom [3 3 2]
        lookat [0 0 -1]
        vup [0 1 0]
        dist-to-focus (vec/length (vec/- lookfrom lookat))
        aperture 2.0
        R (Math/cos (/ Math/PI 4))
        world [(hittable/->Sphere [0 0 -1] 0.5 (material/->Lambertian [0.1 0.2 0.5]))
               (hittable/->Sphere [0 -100.5 -1] 100 (material/->Lambertian [0.8 0.8 0.0]))
               (hittable/->Sphere [1 0 -1] 0.5 (material/->Metal [0.8 0.6 0.2] 0.3))
               (hittable/->Sphere [-1 0 -1] 0.5 (material/->Dialectric 1.5))
               ; An interesting and easy trick with dielectric spheres is to
               ; note that if you use a negative radius, the geometry is
               ; unaffected, but the surface normal points inward. This can be
               ; used as a bubble to make a hollow glass sphere:
               (hittable/->Sphere [-1 0 -1] -0.45 (material/->Dialectric 1.5))]
        cam (camera/make lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus)]
    (raytrace image-width image-height
              (for [j (range (dec image-height) -1 -1)
                    i (range 0 image-width)
                    :let [color (evolve-color world cam image-width image-height num-samples i j max-depth)
                          corrected-color (map #(Math/sqrt %) color)
                          ir (int (* 255.999 (vec/x corrected-color)))
                          ig (int (* 255.999 (vec/y corrected-color)))
                          ib (int (* 255.999 (vec/z corrected-color)))]]
                (pixel-line ir ig ib))
              "./images/background-sphere-with-depth-of-field")))

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
