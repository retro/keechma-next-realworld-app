(ns app.app
  (:require [keechma.next.controllers.router :as router]
            [keechma.next.controllers.subscription]
            [keechma.next.controllers.entitydb]
            [keechma.next.controllers.dataloader]
            [app.controllers.jwt]
            [app.controllers.role]
            [app.controllers.tags]
            [app.controllers.guest.articles]
            [app.controllers.guest.user-actions]
            [app.controllers.guest.login]
            [app.controllers.user.articles]
            ["react-dom" :as rdom]))

(defn homepage? [{:keys [router]}] (= "home" (:page router)))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates
   :keechma/controllers
   {:router {:keechma.controller/params true
             :keechma.controller/type :keechma/router
             :keechma/routes [["" {:page "home"}]
                              ":page"
                              ":page/:subpage"
                              ":page/:subpage/:detail"]}
    :dataloader {:keechma.controller/params true
                 :keechma.controller/type :keechma/dataloader}
    :entitydb {:keechma.controller/params true
               :keechma.controller/type :keechma/entitydb
               :keechma.entitydb/schema {:article {:entitydb/id :slug
                                                   :entitydb/relations {:author :user}}
                                         :user {:entitydb/id :username}}}
    :jwt #:keechma.controller{:params true}
    :role {:keechma.controller/params (fn [_ {:keys [jwt]}] (if jwt :user :guest))
           :keechma.controller/type :keechma/subscription
           :keechma.controller/deps [:jwt]}
    :tags #:keechma.controller {:params homepage?
                                :deps [:router]}}
   :keechma/apps
   {:user {:keechma.app/should-run? (fn [{:keys [role]}] (= :user role))
            :keechma.app/deps [:role]
            :keechma/controllers {:articles #:keechma.controller{:type :user/articles
                                                                 :deps [:router :jwt]
                                                                 :params (fn [deps] (when (homepage? deps) (:router deps)))}}}
    :guest {:keechma.app/should-run? (fn [{:keys [role]}] (= :guest role))
            :keechma.app/deps [:role]
            :keechma/controllers {:articles #:keechma.controller{:type :guest/articles
                                                                 :deps [:router :entitydb :dataloader]
                                                                 :params (fn [deps] (when (homepage? deps) (:router deps)))}
                                  :user-actions #:keechma.controller {:type :guest/user-actions
                                                                      :params true
                                                                      :deps [:router]}
                                  :login #:keechma.controller {:type :guest/login
                                                               :params (fn [{:keys [router]}] (= "login" (:page router)))
                                                               :deps [:router :jwt]}}}}})