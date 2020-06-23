(ns rt-in-weekend.camera
  (:require [rt-in-weekend.ray :as ray]
            [rt-in-weekend.vec :as vec]))

(defn make [lower-left-corner horizontal vertical origin]
  {:lower-left-corner lower-left-corner
   :horizontal horizontal
   :vertical vertical
   :origin origin})

(defn get-ray [{:keys [lower-left-corner horizontal vertical origin]} u v]
  (ray/make origin
            (vec/+ lower-left-corner
                   (vec/+ (vec/* horizontal u)
                          (vec/* vertical v)))))
