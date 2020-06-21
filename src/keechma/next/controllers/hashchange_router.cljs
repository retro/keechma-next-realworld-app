(ns keechma.next.controllers.hashchange-router
  (:require [keechma.next.controller :as ctrl]
            [router.core :as router]
            [goog.events :as events]
            [goog.history.EventType :as EventType])
  (:import goog.History))

(defprotocol IRouterAPI
  (get-url [this params]))

(derive ::controller :keechma/controller)

(defn get-history []
  (History. false nil
            (.getElementById js/document "history_state0")
            (.getElementById js/document "history_iframe0")))

(defn get-route [routes url]
  (let [clean-url (subs url 1)]
    (router/url->map routes clean-url)))

(defn -get-url [routes params]
  (str "#!" (router/map->url routes params)))

(defn bind-listener [ctrl routes]
  (let [history (get-history)
        handler #(ctrl/send-self ctrl :keechma.router.on/route-change (get-route routes (.-token %)))]
    (events/listen history EventType/NAVIGATE handler)
    (.setEnabled history true)
    (fn []
      (events/unlisten history EventType/NAVIGATE handler))))

(defmethod ctrl/init ::controller [ctrl]
  (let [routes (router/expand-routes (:keechma/routes ctrl))]
    (assoc ctrl ::unlisten (bind-listener ctrl routes)
                ::routes routes)))

(defmethod ctrl/api ::controller [ctrl]
  (let [routes (::routes ctrl)]
    (reify
      IRouterAPI
      (get-url [_ params]
        (-get-url routes params)))))

(defmethod ctrl/start ::controller [ctrl]
  (let [url (subs (.. js/window -location -hash) 2)
        routes (::routes ctrl)]
    (router/url->map routes url)))

(defmethod ctrl/receive ::controller [{:keys [state*] :as ctrl} cmd payload]
  (let [routes (::routes ctrl)]
    (case cmd
      :keechma.router.on/route-change (reset! state* payload)
      :back! (.back js/history)
      :redirect! (set! (.-hash js/location) (-get-url routes payload))
      nil)))

(defmethod ctrl/derive-state ::controller [_ state _]
  (:data state))

(defmethod ctrl/terminate ::controller [ctrl]
  (let [unlisten (::unlisten ctrl)]
    (unlisten)))

