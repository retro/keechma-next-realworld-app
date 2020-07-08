(ns keechma.next.helix.template
  (:require [helix.dom :as d]
            [helix.core :refer [$ defnc]])
  (:require-macros [keechma.next.helix.template :refer [defnt]]))

(defn fill-slot [props slot-name slot-content]
  (assoc-in props [::slots slot-name] slot-content))

(defn configure [props configurable-name configurable-content]
  (assoc-in props [::configurables configurable-name] configurable-content))