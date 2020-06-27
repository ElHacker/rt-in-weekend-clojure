(ns rt-in-weekend.camera
  (:require [rt-in-weekend.ray :as ray]
            [rt-in-weekend.vec :as vec]))

(defn degrees-to-radians [degrees]
  (/ (* degrees Math/PI) 180))

; vfov - vertical field of view in degrees.
(defn make [vfov aspect-ratio]
  (let [theta (degrees-to-radians vfov)
        h (Math/tan (/ theta 2))
        viewport-height (* 2.0 h)
        viewport-width (* aspect-ratio viewport-height)
        focal-length 1.0
        origin [0 0 0]
        horizontal [viewport-width 0.0 0.0]
        vertical [0.0 viewport-height 0.0]
        lower-left-corner (vec/- (vec/- (vec/- origin (vec// horizontal 2))
                                        (vec// vertical 2))
                                 [0.0 0.0 focal-length])]
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
