(ns workshop.controllers.emails
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.toolbox.pipeline :as pp :refer-macros [pipeline!]]
            [keechma.next.controllers.pipelines :as pipelines]
            [workshop.domain.emails :refer [get-all]]))

(derive :emails ::pipelines/controller)


(def pipelines
  {:keechma.on/start (pipeline! [value {:keys [state*]}]
                       (pp/preset! state* (get-all)))})

(defmethod ctrl/prep :emails [ctrl]
  (pipelines/register ctrl pipelines))

#_(defmethod ctrl/derive-state :article [_ state {:keys [entitydb]}]
  (edb/get-named entitydb :article/current [(edb/include :author)]))
