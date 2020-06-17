(ns app.app
  (:require [keechma.next.controllers.hashchange-router :as hashchange-router]
            [keechma.next.controllers.subscription]
            [app.controllers.jwt]
            [app.controllers.role]
            [app.controllers.tags]
            [app.controllers.guest.articles]
            ["react-dom" :as rdom]))

(defn homepage? [{:keys [router]}] (= "home" (:page router)))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates
   :keechma/controllers
   {:router {:keechma.controller/params true
             :keechma.controller/type ::hashchange-router/controller
             :keechma/routes [["" {:page "home"}]
                              ":page"
                              ":page/:subpage"
                              ":page/:subpage/:detail"]}
    :jwt #:keechma.controller{:params true}
    :role #:keechma.controller{:params true
                               :deps [:jwt]}
    :tags #:keechma.controller {:params homepage?
                                :deps [:router]}}
   :keechma/apps
   {:guest {:keechma.app/should-run? (fn [{:keys [role]}] (= :guest role))
            :keechma.app/deps [:role]
            :keechma/controllers {:articles #:keechma.controller{:type :guest/articles
                                                                 :deps [:router]
                                                                 :params (fn [deps]
                                                                           (when (homepage? deps)
                                                                             (:router deps)))}}}}})