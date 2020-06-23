(ns keechma.next.controllers.entitydb.controller
  (:require [entitydb.entitydb :as edb]
            [keechma.next.controllers.entitydb.protocols :refer [IEntityDbApi]]
            [keechma.next.controller :as ctrl]
            [cljs.core.async :refer [<! timeout chan alts! close!]]
            [goog.object :as gobj])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

;; TODO: Remove when EDB is normalized with new names
(def insert-entity edb/insert)
(def insert-many edb/insert-many)
(def insert-named edb/insert-named-item)
(def insert-collection edb/insert-collection)
(def remove-entity edb/remove-by-id)
(def remove-named edb/remove-named-item)
(def remove-collection edb/remove-collection)
(def get-entity edb/get-by-id)
(def get-named edb/get-named-item)
(def get-collection edb/get-collection)

(deftype EntityDbApi [ctrl state*]
  IEntityDbApi
  (insert-entity! [_ entity-type data]
    (ctrl/transact ctrl #(swap! state* insert-entity entity-type data))
    nil)
  (insert-many! [_ entity-type entities]
    (ctrl/transact ctrl #(swap! state* insert-many entity-type entities))
    nil)
  (insert-named! [_ entity-type entity-name data]
    (ctrl/transact ctrl #(swap! state* insert-named entity-type entity-name data))
    nil)
  (insert-named! [_ entity-type entity-name data n-meta]
    (ctrl/transact ctrl #(swap! state* insert-named entity-type entity-name data n-meta))
    nil)
  (insert-collection! [_ entity-type collection-name data]
    (ctrl/transact ctrl #(swap! state* insert-collection entity-type collection-name data))
    nil)
  (insert-collection! [_ entity-type collection-name data c-meta]
    (ctrl/transact ctrl #(swap! state* insert-collection entity-type collection-name data c-meta))
    nil)
  (remove-entity! [_ entity-type id]
    (ctrl/transact ctrl #(swap! state* remove-entity entity-type id))
    nil)
  (remove-named! [_ entity-name]
    (ctrl/transact ctrl #(swap! state* remove-named entity-name))
    nil)
  (remove-collection! [_ collection-name]
    (ctrl/transact ctrl #(swap! state* remove-collection collection-name))
    nil))

(defn request-idle-callback-chan! []
  (let [cb-chan (chan)]
    (if-let [req-idle-cb (gobj/get js/window "requestIdleCallback")]
      (req-idle-cb #(close! cb-chan))
      (close! cb-chan))
    cb-chan))

(defn start-vacuum! [{:keys [state*] :as ctrl}]
  (let [interval (or (:keechma.entitydb/vacuum-interval ctrl) (* 10 60 1000)) ;; Vacuum EntityDB every 10 minutes
        poison-chan (chan)]
    (go-loop []
             (let [[_ c] (alts! [poison-chan (timeout interval)])]
               (when-not (= c poison-chan)
                 (<! (request-idle-callback-chan!))
                 (swap! state* edb/vacuum)
                 (recur))))
    (fn []
      (close! poison-chan))))

(defmethod ctrl/init :keechma/entitydb [ctrl]
  (assoc ctrl :keechma.entitydb.vacuum/stop! (start-vacuum! ctrl)))

(defmethod ctrl/api :keechma/entitydb [{:keys [state*] :as ctrl}]
  (->EntityDbApi ctrl state*))

(defmethod ctrl/start :keechma/entitydb [ctrl _ _ _]
  (edb/insert-schema {} (:keechma.entitydb/schema ctrl)))

(defmethod ctrl/terminate :keechma/entitydb [ctrl]
  (when-let [stop-vacuum! (:keechma.entitydb.vacuum/stop! ctrl)]
    (stop-vacuum!)))