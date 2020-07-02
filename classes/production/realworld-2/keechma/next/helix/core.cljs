(ns keechma.next.helix.core
  (:require
    [clojure.string :as string]
    [helix.core :as hx :refer [$ <>]]
    [helix.dom :as d]
    [helix.hooks :as hooks]
    [keechma.next.helix.lib :refer [defnc]]
    ["react" :as react]
    ["react-dom" :as rdom]
    [keechma.next.core :as keechma]
    [keechma.next.controller :as ctrl]
    [goog.object :as gobj]
    [cljs-bean.core :refer [bean]]
    [clojure.string :as str]))

(def keechma-app-context (react/createContext nil))

(defnc KeechmaRoot [{:keys [keechma/app children]}]
  (hx/provider
    {:context keechma-app-context
     :value app}
    (when app
      children)))

;;https://github.com/roman01la/uix/blob/master/core/src/uix/hooks/alpha.cljc#L208
(defn make-sub [data-or-meta]
  (fn sub
    ([app controller] (sub app controller identity))
    ([app controller processor]
     (let [subscribe! (if (= :data data-or-meta) keechma/subscribe keechma/subscribe-meta)
           get-state (if (= :data data-or-meta) keechma/get-derived-state keechma/get-meta-state)
           processor' (or processor identity)
           get-current-value
           (hooks/use-callback
             [processor' get-state (pr-str controller) (keechma/get-id app)]
             (comp processor' (partial get-state app controller)))

           get-initial-state
           (hooks/use-callback
             [get-current-value subscribe!]
             #js {:get-current-value get-current-value
                  :subscribe subscribe!
                  :value (get-current-value)})

           batcher
           (hooks/use-callback
             [(keechma/get-id app)]
             (keechma/get-batcher app))

           [state set-state] (hooks/use-state get-initial-state)

           ret-value
           (if (or (not (identical? (gobj/get state "get-current-value") get-current-value))
                   (not (identical? (gobj/get state "subscribe") subscribe!)))
             (let [ret-val (get-current-value)]
               (set-state #js {:get-current-value get-current-value
                               :subscribe subscribe!
                               :value ret-val})
               ret-val)
             (gobj/get state "value"))]

       (react/useDebugValue ret-value)

       (hooks/use-effect
         [get-current-value subscribe! (keechma/get-id app)]
         (let [did-unsubscribe? (volatile! false)
               check-for-updates
               (fn []
                 (when-not ^boolean @did-unsubscribe?
                   (let [value (get-current-value)]
                     (batcher
                       (fn []
                         (set-state
                           (fn [v]
                             (if (or (not (identical? (gobj/get v "get-current-value") get-current-value))
                                     (not (identical? (gobj/get v "subscribe") subscribe!))
                                     (= (gobj/get v "value") value))
                               v
                               (.assign js/Object #js {} v #js {:value value})))))))))
               unsubscribe! (subscribe! app controller check-for-updates)]
           (check-for-updates)
           (fn []
             (vreset! did-unsubscribe? true)
             (unsubscribe!))))
       ret-value))))

(defn with-keechma [Component]
  (-> (fn KeechmaHOC [props ref]
        (let [app (hooks/use-context keechma-app-context)]
          ($ Component
             {:keechma/app app
              :ref ref
              & (helix.core/extract-cljs-props props)})))
      react/forwardRef))

(defn make-keechma-app-fn [keechma-app-fn]
  (fn [props & args]
    (if-let [app (get props :keechma/app)]
      (apply keechma-app-fn app args)
      (throw (ex-info "Missing `:keechma/app` key. Did you forget to wrap the component with `with-keechma` HOC?"
                      {:props props})))))

(def use-sub (make-keechma-app-fn (make-sub :data)))
(def use-meta-sub (make-keechma-app-fn (make-sub :meta)))
(def dispatch (make-keechma-app-fn keechma/dispatch))
(def broadcast (make-keechma-app-fn keechma/broadcast))
(def call (make-keechma-app-fn keechma/call))
(def get-api* (make-keechma-app-fn keechma/get-api*))