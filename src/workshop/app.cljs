(ns workshop.app
  (:require
    [workshop.controllers.emails]
    [workshop.controllers.emails-list]
    [workshop.controllers.current-email]
    [workshop.controllers.current-thread]
    [workshop.controllers.compose-form]
    [workshop.controllers.replying-to]
    [workshop.controllers.current-email-reply-form]
    [keechma.next.controllers.router]
    ["react-dom" :as rdom]))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates
   :keechma/controllers
   {:router {:keechma.controller/params true
             :keechma.controller/type :keechma/router
             :keechma/routes [["" {:page "inbox"}]
                              ":page"
                              ":page/:id"]}
    :emails #:keechma.controller {:params true}
    :emails-list #:keechma.controller {:params (fn [{:keys [router]}]
                                                 (:page router))
                                       :deps [:router :emails]}
    :current-email #:keechma.controller {:params (fn [{:keys [router]}]
                                                   (:id router))
                                         :deps [:router :emails]}
    :current-thread #:keechma.controller {:params (fn [{:keys [router]}]
                                                    (:id router))
                                          :deps [:router :emails]}}
   :keechma/apps {}})