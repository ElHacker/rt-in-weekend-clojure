(ns rt-in-weekend.camera
  (:require [rt-in-weekend.ray :as ray]
            [rt-in-weekend.vec :as vec]))

(defn degrees-to-radians [degrees]
  (/ (* degrees Math/PI) 180))

; vfov - vertical field of view in degrees.
; vup - view up
(defn make [lookfrom lookat vup vfov aspect-ratio]
  (let [theta (degrees-to-radians vfov)
        h (Math/tan (/ theta 2))
        viewport-height (* 2.0 h)
        viewport-width (* aspect-ratio viewport-height)
        w (vec/unit-vector (vec/- lookfrom lookat))
        u (vec/unit-vector (vec/cross vup w))
        v (vec/cross w u)
        origin lookfrom
        horizontal (vec/* u viewport-width)
        vertical (vec/* v viewport-height)
        lower-left-corner (vec/-
                            (vec/-
                              (vec/-
                                origin
                                (vec// horizontal 2))
                              (vec// vertical 2))
                            w)]
    {:lower-left-corner lower-left-corner
     :horizontal horizontal
     :vertical vertical
     :origin origin}))

(defn get-ray [{:keys [lower-left-corner horizontal vertical origin]} u v]
  (ray/make origin
            (vec/- (vec/+ lower-left-corner
                          (vec/+ (vec/* horizontal u)
                                 (vec/* vertical v)))
                   origin)))
