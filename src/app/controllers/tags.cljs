(ns app.controllers.tags
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]))

(derive :tags ::pipelines/controller)

(def pipelines
  {:keechma.on/start (pipeline! [value {:keys [state*]}]
                       (api/get-tags)
                       (preset! state* value))})

(defmethod ctrl/prep :tags [ctrl]
  (pipelines/register ctrl pipelines))