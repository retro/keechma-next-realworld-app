(ns app.controllers.user.user-actions
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [app.settings :as settings]))

(derive :user/user-actions ::pipelines/controller)

(def pipelines
  {:logout (pipeline! [value ctrl]
             (ctrl/dispatch ctrl :jwt :clear)
             (ctrl/dispatch ctrl :router :redirect! {:page "home"}))})

(defmethod ctrl/prep :user/user-actions [ctrl]
  (pipelines/register ctrl pipelines))
