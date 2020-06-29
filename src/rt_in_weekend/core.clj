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

(defn make-world []
  (let [world (atom [(hittable/->Sphere [0 -1000 0] 1000 (material/->Lambertian [0.5 0.5 0.5]))])
        drand #(* (rand) (rand))]
    (doseq [a (range -11 11)
            b (range -11 11)
            :let [choose-mat (rand)
                  center [(+ a (* 0.9 (rand))) 0.2 (+ b (* 0.9 (rand)))]]]
      (if (> (vec/length (vec/- center [4 0.2 0])) 0.9)
        (cond
          (< choose-mat 0.8) ; diffuse
          (swap! world conj (hittable/->Sphere center 0.2 (material/->Lambertian [(drand) (drand) (drand)])))
          (< choose-mat 0.95) ;metal
          (swap! world conj (hittable/->Sphere center 0.2 (material/->Metal [(* 0.5 (inc (rand)))
                                                                               (* 0.5 (inc (rand)))
                                                                               (* 0.5 (rand))]
                                                                               (rand))))
          :else ;glass
          (swap! world conj (hittable/->Sphere center 0.2 (material/->Dialectric 1.5)))
          )))
    (swap! world conj (hittable/->Sphere [0 1 0] 1.0 (material/->Dialectric 1.5)))
    (swap! world conj (hittable/->Sphere [-4 1 0] 1.0 (material/->Lambertian [0.4 0.2 0.1])))
    (swap! world conj (hittable/->Sphere [4 1 0] 1.0 (material/->Metal [0.7 0.6 0.5] 0.0)))
    @world))

(defn final-scene []
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 384
        image-height (int (/ image-width aspect-ratio))
        num-samples 30
        max-depth 50
        lookfrom [13 2 3]
        lookat [0 0 0]
        vup [0 1 0]
        dist-to-focus 10.0
        aperture 0.1
        world (make-world)
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
              "./images/final-scene")))


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

(time (final-scene))
