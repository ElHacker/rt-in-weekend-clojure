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
