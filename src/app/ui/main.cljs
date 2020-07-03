(ns app.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]
            [app.ui.components.header :refer [Header]]
            [app.ui.components.footer :refer [Footer]]
            [app.ui.pages.home :refer [Home]]
            [app.ui.pages.login :refer [Login]]
            [app.ui.pages.article :refer [Article]]
            [app.ui.pages.profile :refer [Profile]]
            [app.ui.pages.settings :refer [Settings]]
            [app.ui.pages.register :refer [Register]]
            ))

(defnc MainRenderer
  [props]
  (let [{:keys [page]} (use-sub props :router)]
    (d/div
      ($ Header)
      (case page
        "home" ($ Home)
        "login" ($ Login)
        "article" ($ Article)
        "profile" ($ Profile)
        "settings" ($ Settings)
        "register" ($ Register)
        (d/div "404"))
      ($ Footer))))

(def Main (with-keechma MainRenderer))