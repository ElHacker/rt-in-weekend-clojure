(ns rt-in-weekend.camera
  (:require [rt-in-weekend.ray :as ray]
            [rt-in-weekend.vec :as vec]))

(defn degrees-to-radians [degrees]
  (/ (* degrees Math/PI) 180))

(defn random-in-unit-disk []
  (loop [p (vec/- (vec/* [(rand) (rand) 0] 2) [1 1 0])]
    (if (< (vec/length-squared p) 1)
      p
      (recur (vec/- (vec/* [(rand) (rand) 0] 2) [1 1 0])))))

; vfov - vertical field of view in degrees.
; vup - view up
(defn make [lookfrom lookat vup vfov aspect-ratio aperture focus-dist]
  (let [theta (degrees-to-radians vfov)
        h (Math/tan (/ theta 2))
        viewport-height (* 2.0 h)
        viewport-width (* aspect-ratio viewport-height)
        w (vec/unit-vector (vec/- lookfrom lookat))
        u (vec/unit-vector (vec/cross vup w))
        v (vec/cross w u)
        origin lookfrom
        horizontal (vec/* (vec/* u viewport-width) focus-dist)
        vertical (vec/* (vec/* v viewport-height) focus-dist)
        lower-left-corner (vec/-
                            (vec/-
                              (vec/-
                                origin
                                (vec// horizontal 2))
                              (vec// vertical 2))
                            (vec/* w focus-dist))
        lens-radius (/ aperture 2.0)]
    {:lower-left-corner lower-left-corner
     :horizontal horizontal
     :vertical vertical
     :origin origin
     :lens-radius lens-radius
     :u u
     :v v
     :w w}))

(defn get-ray [{:keys [lower-left-corner horizontal vertical origin lens-radius u v]} s t]
  (let [rd (vec/* (random-in-unit-disk) lens-radius)
        offset (vec/+ (vec/* u (vec/x rd)) (vec/* v (vec/y rd)))]
    (ray/make (vec/+ origin offset)
              (vec/- (vec/- (vec/+ lower-left-corner
                                   (vec/+ (vec/* horizontal s)
                                          (vec/* vertical t)))
                            origin)
                     offset))))
