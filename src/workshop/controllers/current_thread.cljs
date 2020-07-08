(ns workshop.controllers.current-thread
  (:require [keechma.next.controller :as ctrl]))

(derive :current-thread :keechma/controller)

(defmethod ctrl/derive-state :current-thread [ctrl _ {:keys [emails]}]
  (let [id (:keechma.controller/params ctrl)]
    (filterv #(= id (:thread-id %)) emails)))