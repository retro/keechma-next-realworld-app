(ns app.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]
            [app.ui.components.footer :refer [Footer]]
            [app.ui.pages.home :refer [Home]]
            [app.ui.pages.login :refer [Login]]
            [app.ui.pages.article :refer [Article]]))

(defnc MainRenderer
  [props]
  (let [{:keys [page]} (use-sub props :router)]
    (d/div
      (case page
        "home" ($ Home)
        "login" ($ Login)
        "article" ($ Article)
        (d/div "404"))
      ($ Footer))))

(def Main (with-keechma MainRenderer))
