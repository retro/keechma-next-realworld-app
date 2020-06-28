(ns app.controllers.guest.login-form
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [keechma.next.controllers.form :as form]
            [app.validators :as v]
            [promesa.core :as p]))

(derive :guest/login-form ::pipelines/controller)

(def pipelines
  {:keechma.form/get-data (pipeline! [value ctrl]
                            {:email "conduit123@mailinator.com"
                             :password "1234567890"})
   :keechma.form/submit-data (pipeline! [value ctrl]
                               (api/login value)
                               (ctrl/broadcast ctrl :guest/login value)
                               (ctrl/dispatch ctrl :router :redirect! {:page "home" :subpage "personal"}))})

(defmethod ctrl/prep :guest/login-form [ctrl]
  (pipelines/register ctrl (form/wrap pipelines (v/to-validator {:email [:email :not-empty]
                                                                 :password [:not-empty :ok-password]}))))
