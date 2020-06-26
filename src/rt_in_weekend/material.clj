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
      (vec/- in-unit-sphere))))

;Now real glass has reflectivity that varies with angle — look at a window at a
;steep angle and it becomes a mirror. There is a big ugly equation for that,
;but almost everybody uses a cheap and surprisingly accurate polynomial
;approximation by Christophe Schlick
(defn schlick [cosine ref-idx]
  (let [r0 (/ (- 1 ref-idx) (+ 1 ref-idx))
        r0 (* r0 r0)]
    (+ r0
       (* (- 1 r0)
          (Math/pow (- 1 cosine) 5)))))

(defprotocol Material
  (scatter [this r-in rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter [this r-in rec]
    (let [target (vec/+ (vec/+ (:p rec) (:normal rec)) (random-unit-vector))
          scattered (ray/make (:p rec) (vec/- target (:p rec)))]
      {:ok true :attenuation (:albedo this) :scattered scattered})))

(defrecord Metal [albedo f]
  Material
  (scatter [this r-in rec]
    (let [fuzz (if (< f 1) f 1)
          reflected (vec/reflect (vec/unit-vector (:direction r-in)) (:normal rec))
          scattered (ray/make (:p rec) (vec/+ reflected (vec/* (random-in-unit-sphere) fuzz)))
          final (vec/dot (:direction scattered) (:normal rec))]
      {:ok (pos? final) :attenuation (:albedo this) :scattered scattered})))

(defrecord Dialectric [ref-idx]
  Material
  (scatter [this r-in rec]
    (let [attenuation [1.0 1.0 1.0]
          etai-over-etat (if (:front-face rec) (/ 1.0 ref-idx) ref-idx)
          unit-direction (vec/unit-vector (:direction r-in))
          cos-theta (min (vec/dot (vec/- unit-direction) (:normal rec)) 1.0)
          sin-theta (Math/sqrt (- 1.0 (* cos-theta cos-theta)))
          can-refract (<= (* etai-over-etat sin-theta) 1.0)
          refracted-or-reflected (if can-refract
                      (vec/refract unit-direction (:normal rec) etai-over-etat)
                      (vec/reflect unit-direction (:normal rec)))
          scattered (ray/make (:p rec) refracted-or-reflected)
          reflect-prob (schlick cos-theta etai-over-etat)]
      (if (< (rand) reflect-prob)
        {:ok true :attenuation attenuation :scattered (ray/make
                                                        (:p rec)
                                                        (vec/reflect unit-direction (:normal rec)))}
        {:ok true :attenuation attenuation :scattered scattered}))))
