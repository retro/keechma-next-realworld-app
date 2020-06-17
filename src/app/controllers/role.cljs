(ns app.controllers.role
  (:require [keechma.next.controller :as ctrl]))

(derive :role :keechma/controller)

(defmethod ctrl/derive-state :role [_ _ {:keys [jwt]}]
  (if jwt :user :guest))