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

(defnc KeechmaRoot [{:keys [keechma/app children] :as props}]
  (hx/provider
    {:context keechma-app-context
     :value app}
    children))

;;https://github.com/roman01la/uix/blob/master/core/src/uix/hooks/alpha.cljc#L208
(defn make-sub [data-or-meta]
  (fn sub
    ([context controller] (sub context controller identity))
    ([{:keys [batcher] :as context} controller processor]
     (let [subscribe! (if (= :data data-or-meta) (get context :subscribe!) (get context :subscribe-meta!))
           get-state (if (= :data data-or-meta) (get context :get-derived-state) (get context :get-meta-state))
           processor' (or processor identity)
           get-current-value
           (hooks/use-callback
             [processor' get-state (pr-str controller)]
             (comp processor' (partial get-state controller)))

           get-initial-state
           (hooks/use-callback
             [get-current-value subscribe!]
             #js {:get-current-value get-current-value
                  :subscribe subscribe!
                  :value (get-current-value)})

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
         [get-current-value subscribe!]
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
               unsubscribe! (subscribe! controller check-for-updates)]
           (check-for-updates)
           (fn []
             (vreset! did-unsubscribe? true)
             (unsubscribe!))))
       ret-value))))

(defn with-keechma [Component]
  (-> (fn KeechmaHOC [props ref]
        (let [context (hooks/use-context keechma-app-context)
              use-sub (partial (make-sub :data) context)
              use-meta-sub (partial (make-sub :meta) context)]
          ($ Component
             {:keechma/context context
              :keechma/use-sub use-sub
              :keechma/use-meta-sub use-meta-sub
              :keechma/send! (:send! context)
              :ref ref
              & (helix.core/extract-cljs-props props)})))
      react/forwardRef))