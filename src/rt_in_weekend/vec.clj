(ns rt-in-weekend.vec
  (:require [clojure.core :as clj]))

(defn mute [op
            [^float x1 ^float y1 ^float z1]
            [^float x2 ^float y2 ^float z2]]
  [(op x1 x2) (op y1 y2) (op z1 z2)])

(defn + [v1 v2]
  (mute clj/+ v1 v2))


(defn * [v1 v2]
  (if (number? v2)
    (map #(clj/* v2 %) v1)
    (mute clj/* v1 v2)))

(defn -
  ([v] (* v -1))
  ([v1 v2] (mute clj/- v1 v2)))

(defn / [v1 v2]
  (if (number? v2)
    (if (zero? v2)
      1
      (* v1 (clj// 1 v2)))
    (mute clj// v1 v2)))

(defn dot [v1 v2]
  (reduce clj/+ (* v1 v2)))

(defn cross [[x1 y1 z1] [x2 y2 z2]]
  [(clj/- (clj/* y1 z2) (clj/* z1 y2))
   (clj/- (clj/* z1 x2) (clj/* x1 z2))
   (clj/- (clj/* x1 y2) (clj/* y1 x2))])

(defn length-squared [v]
  (reduce clj/+ (map #(clj/* % %) v)))

(defn length [v]
  (Math/sqrt (length-squared v)))

(defn unit-vector [v]
  (let [l (length v)]
    (map #(clj// % l) v)))

(defn x [v]
  (first v))

(defn y [v]
  (second v))

(defn z [v]
  (last v))

(defn reflect [v n]
  (let [m (* n (clj/* (dot v n) 2))]
    (- v m)))

(defn refract [uv n etai-over-etat]
  (let [cos-theta (dot (- uv) n)
        r-out-parallel (* (+ uv (* n cos-theta)) etai-over-etat)
        r-out-perp (* n (clj/- (Math/sqrt (clj/- 1 (length-squared r-out-parallel)))))]
    (+ r-out-parallel r-out-perp)))
