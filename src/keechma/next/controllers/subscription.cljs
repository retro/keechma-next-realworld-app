(ns keechma.next.controllers.subscription
  (:require [keechma.next.controller :as ctrl]))

(derive :keechma/subscription :keechma/controller)

(defmethod ctrl/derive-state :keechma/subscription [{:keys [subscription]} state deps-state]
  (when subscription
    (subscription state deps-state)))
