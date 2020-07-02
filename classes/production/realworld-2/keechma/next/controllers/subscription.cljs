(ns keechma.next.controllers.subscription
  (:require [keechma.next.controller :as ctrl]))

(derive :keechma/subscription :keechma/controller)

(defmethod ctrl/derive-state :keechma/subscription [ctrl state deps-state]
  (if-let [derive-state (:keechma.subscription/derive-state ctrl)]
    (derive-state state deps-state)
    (:keechma.controller/params ctrl)))
