(ns rt-in-weekend.hittable
  (:require [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]))

(defprotocol Hittable
  (hit [this r t-min t-max]))

(defn hit-record [r t center radius]
  (let [p (ray/point-at r t)
        outward-normal (/ (- p center) radius)
        front-face (< (vec/dot (:direction r) outward-normal) 0)]
    {:t t :p p :normal (if front-face outward-normal (- outward-normal))}))

(defrecord Sphere [center radius]
  Hittable
  (hit [this r t-min t-max]
    (let [oc (vec/- (ray/origin r) (:center this))
          a (vec/length-squared (ray/direction r))
          half-b (vec/dot oc (ray/direction r))
          c (- (vec/length-squared oc) (* (:radius this) (:radius this)))
          discriminant (- (* half-b half-b) (* a c))]
      (when (pos? discriminant)
        (let [root (Math/sqrt discriminant)
              temp (/ (- (- half-b) root) a)]
          (if (and (< temp t-max) (> temp t-min))
            (hit-record r temp (:center this) (:radius this))
            (let [temp (/ (+ (- half-b) root) a)]
              (when (and (< temp t-max) (> temp t-min))
                (hit-record r temp (:center this) (:radius this))))))))))
