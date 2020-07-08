(ns workshop.ui.components.email-thread
  (:require [keechma.next.helix.core :refer [with-keechma use-sub]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]
            [keechma.next.controllers.router :as router]))

(defnc EmailThreadRenderer [{:keys [list] :as props}]
  (let [route (use-sub props :router)
        email (use-sub props :current-email)]
    (d/div
      (d/a {:href (router/get-url props :router (dissoc route :id))} "[Close]")
      (d/hr)
      (d/h5 (:from email))
      (d/h3 (:subject email))
      (d/p (:body email)))))

(def EmailThread (with-keechma EmailThreadRenderer))