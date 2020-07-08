(ns workshop.controllers.current-email-reply-form
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [keechma.next.controllers.form :as form]
            [app.validators :as v]
            [promesa.core :as p]
            [workshop.domain.emails :as emails :refer [logged-in-email]]))

(derive :current-email-reply-form ::pipelines/controller)

(defn send [email]
  (emails/send email)
  nil)

(def pipelines
  {:keechma.form/get-data (pipeline! [value {:keys [deps-state*] :as ctrl}]
                            {:thread-id (get-in @deps-state* [:router :id])
                             :from logged-in-email})
   :keechma.form/submit-data (pipeline! [value ctrl]
                               (let [id (str (gensym 'email-id))]
                                 (pipeline! [value ctrl]
                                   (send (assoc value :id id))
                                   (ctrl/dispatch ctrl :router :redirect-to {:page "sent" :id id}))))})

(defmethod ctrl/prep :current-email-reply-form [ctrl]
  (pipelines/register ctrl (form/wrap pipelines (v/to-validator {:to [:email :not-empty]
                                                                 :body [:not-empty]}))))
