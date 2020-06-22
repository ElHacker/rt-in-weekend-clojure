(ns rt-in-weekend.ray
  (:require [rt-in-weekend.vec :as vec]))

(defn make [origin direction]
  {:origin origin :direction direction})

(defn origin [ray]
  (:origin ray))

(defn direction [ray]
  (:direction ray))

(defn point-at [ray t]
  (vec/+ (origin ray)
         (vec/* (direction ray) t)))

(defn hit-sphere [center radius {:keys [origin direction]}]
  (let [oc (vec/- origin center)
        a (vec/length-squared direction)
        half_b (vec/dot oc direction)
        c (- (vec/length-squared oc) (* radius radius))
        discriminant (- (* half_b half_b) (* a c))]
    (if (neg? discriminant)
      -1.0
      (/ (- (- half_b)
            (Math/sqrt discriminant))
         a))))
