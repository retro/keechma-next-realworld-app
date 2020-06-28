(ns app.app
  (:require [keechma.next.controllers.router]
            [keechma.next.controllers.subscription]
            [keechma.next.controllers.entitydb]
            [keechma.next.controllers.dataloader]
            [app.controllers.jwt]
            [app.controllers.tags]
            [app.controllers.articles]
            [app.controllers.article]
            [app.controllers.current-user]
            [app.controllers.comments]
            [app.controllers.guest.user-actions]
            [app.controllers.guest.login-form]
            ["react-dom" :as rdom]))

(defn page-eq? [page]
  (fn [{:keys [router]}]
    (= page (:page router))))

(defn role-eq? [role]
  (fn [deps]
    (= role (:role deps))))

(def homepage? (page-eq? "home"))

(defn slug [{:keys [router]}]
  (:slug router))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates
   :keechma/controllers
   {:router {:keechma.controller/params true
             :keechma.controller/type :keechma/router
             :keechma/routes [["" {:page "home"}]
                              ":page"
                              ":page/:subpage"
                              ":page/:subpage/:detail"
                              ["profile/:user" {:page "profile"}]
                              ["article/:slug" {:page "article"}]
                              ["tag/:tag" {:page "home"}]]}
    :dataloader {:keechma.controller/params true
                 :keechma.controller/type :keechma/dataloader}
    :entitydb {:keechma.controller/params true
               :keechma.controller/type :keechma/entitydb
               :keechma.entitydb/schema {:article {:entitydb/id :slug
                                                   :entitydb/relations {:author :user}}
                                         :comment {:entitydb/relations {:author :user}}
                                         :user {:entitydb/id :username}}}
    :jwt #:keechma.controller {:params true}
    :role #:keechma.controller {:params (fn [{:keys [jwt]}] (if jwt :user :guest))
                                :type :keechma/subscription
                                :deps [:jwt]}
    :current-user #:keechma.controller {:params true
                                        :deps [:jwt :dataloader :entitydb]}
    :tags #:keechma.controller {:params homepage?
                                :deps [:router :dataloader]}
    :articles #:keechma.controller{:deps [:router :jwt :entitydb :dataloader]
                                   :params homepage?}
    :article #:keechma.controller {:deps [:router :jwt :entitydb :dataloader]
                                   :params slug}
    :comments #:keechma.controller {:deps [:entitydb :dataloader :router]
                                    :params slug}}
   :keechma/apps
   {#_#_:user {:keechma.app/should-run? (fn [{:keys [role]}] (= :user role))
               :keechma.app/deps [:role]}
    :guest {:keechma.app/should-run? (role-eq? :guest)
            :keechma.app/deps [:role]
            :keechma/controllers {:user-actions #:keechma.controller {:type :guest/user-actions
                                                                      :params true
                                                                      :deps [:router]}
                                  :login-form #:keechma.controller {:type :guest/login-form
                                                                    :params (page-eq? "login")
                                                                    :deps [:router :jwt]}}}}})