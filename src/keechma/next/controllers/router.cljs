(ns keechma.next.controllers.router
  (:require [keechma.next.controller :as ctrl]
            [router.core :as router]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [keechma.next.protocols :as keechma-pt]
            [keechma.next.controllers.router.protocols :as pt :refer [IRouterApi]])
  (:import goog.History))

(derive :keechma/router :keechma/controller)

(defn get-history []
  (History. false nil
            (.getElementById js/document "history_state0")
            (.getElementById js/document "history_iframe0")))

(defn get-route [routes url]
  (let [clean-url (subs url 1)]
    (router/url->map routes clean-url)))

(defn map->url [routes params]
  (str "#!" (router/map->url routes params)))

(defn bind-listener [ctrl routes]
  (let [history (get-history)
        handler #(ctrl/handle ctrl :keechma.router.on/route-change (get-route routes (.-token %)))]
    (events/listen history EventType/NAVIGATE handler)
    (.setEnabled history true)
    (fn []
      (events/unlisten history EventType/NAVIGATE handler))))

(defmethod ctrl/init :keechma/router [ctrl]
  (let [routes (router/expand-routes (:keechma/routes ctrl))]
    (assoc ctrl ::unlisten (bind-listener ctrl routes)
                ::routes routes)))

(defmethod ctrl/api :keechma/router [ctrl]
  (let [routes (::routes ctrl)]
    (reify
      IRouterApi
      (get-url [_ params]
        (map->url routes params)))))

(defmethod ctrl/start :keechma/router [ctrl]
  (let [url (subs (.. js/window -location -hash) 2)
        routes (::routes ctrl)]
    (router/url->map routes url)))

(defmethod ctrl/handle :keechma/router [{:keys [state*] :as ctrl} cmd payload]
  (let [routes (::routes ctrl)]
    (case cmd
      :keechma.router.on/route-change (reset! state* payload)
      :back! (.back js/history)
      :redirect!  (set! (.-hash js/location) (map->url routes payload))
      nil)))

(defmethod ctrl/derive-state :keechma/router [_ state _]
  (:data state))

(defmethod ctrl/terminate :keechma/router [ctrl]
  (let [unlisten (::unlisten ctrl)]
    (unlisten)))

(defn make-api-proxy [api-fn]
  (fn [{:keechma/keys [app]} controller-name & args]
    (let [api* (keechma-pt/-get-api* app controller-name)]
      (apply api-fn @api* args))))

(def get-url (make-api-proxy pt/get-url))