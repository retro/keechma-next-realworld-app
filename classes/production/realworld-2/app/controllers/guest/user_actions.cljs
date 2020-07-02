(ns app.controllers.guest.user-actions
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [app.settings :as settings]))

(derive :guest/user-actions ::pipelines/controller)

(def pipelines
  {:toggle-favorite (pipeline! [value {:keys [] :as ctrl}]
                      (ctrl/dispatch ctrl :router :redirect! {:page "login"}))})

(defmethod ctrl/prep :guest/user-actions [ctrl]
  (pipelines/register ctrl pipelines))
