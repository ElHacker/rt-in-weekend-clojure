(ns rt-in-weekend.material
  (:require [rt-in-weekend.vec :as vec]
            [rt-in-weekend.ray :as ray]))

; Diffuse material hack
(defn random-in-unit-sphere []
  (let [random-vec #(vec/-
                      (vec/* [(rand) (rand) (rand)] 2.0)
                      [1.0 1.0 1.0])
        p (atom nil)]
    (do
      (reset! p (random-vec))
      (while (>= (vec/length-squared @p) 1.0)
        (reset! p (random-vec))))
    @p))

; Lambertian diffuse aproximation
(defn random-unit-vector []
  (let [a (* 2 Math/PI (rand))
        z (+ (- 1) (* 2 (rand)))
        r (Math/sqrt (- 1 (* z z)))]
    [(* r (Math/cos a)) (* r (Math/sin a)) z]))

; Hemispherical scattering
(defn random-in-hemisphere [normal]
  (let [in-unit-sphere (random-in-unit-sphere)]
    (if (> (vec/dot in-unit-sphere normal) 0.0) ; in the same hemisphere as the normal
      in-unit-sphere
      (vec/* in-unit-sphere -1))))

(defprotocol Material
  (scatter [this r-in rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter [this r-in rec]
    (let [target (vec/+ (vec/+ (:p rec) (:normal rec)) (random-unit-vector))
          scattered (ray/make (:p rec) (vec/- target (:p rec)))]
      {:ok true :attenuation (:albedo this) :scattered scattered})))

(defrecord Metal [albedo]
  Material
  (scatter [this r-in rec]
    (let [reflected (vec/reflect (vec/unit-vector (:direction r-in)) (:normal rec))
          scattered (ray/make (:p rec) reflected)
          final (vec/dot (:direction scattered) (:normal rec))]
      {:ok (pos? final) :attenuation (:albedo this) :scattered scattered})))
