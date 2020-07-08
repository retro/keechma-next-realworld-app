(ns workshop.controllers.replying-to
  (:require [keechma.next.controller :as ctrl]))

(derive :replying-to :keechma/controller)

(defmethod ctrl/start :replying-to [_ _ _ _]
  false)

(defmethod ctrl/handle :replying-to [{:keys [state*]} event payload]
  (case event
    :reply-to (reset! state* true)
    :discard-reply-to (reset! state* false)
    :keechma.on/deps-change
    (when (and (contains? payload :router)
               (not (get-in payload [:router :id])))
      (reset! state* false))
    nil))