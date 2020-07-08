(ns workshop.controllers.current-email
  (:require [keechma.next.controller :as ctrl]))

(derive :current-email :keechma/controller)

(defmethod ctrl/derive-state :current-email [ctrl _ {:keys [emails]}]
  (let [id (:keechma.controller/params ctrl)]
    (first (filter #(= id (:id %)) emails))))