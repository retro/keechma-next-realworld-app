(ns keechma.next.controller
  (:require [keechma.next.protocols :as protocols]))

(declare transact)

(defn controller-dispatcher
  "Dispatcher for controller multimethods that receive the controller config map as it's first argument. Returns the
  value of the `:keechma/controller` key in the first argument"
  [c & _]
  (:keechma.controller/type c))

(defn transacted-controller-dispatcher [c & _]
  (if (:keechma/is-transacting c)
    (:keechma.controller/type c)
    :keechma.controller/wrap-transaction))

(defmulti prep
          "Prep is called once when the app is started (for each controller)"
          controller-dispatcher)

(defmulti init
          "Called before the controller is started. It will receive a controller map. This map will contain controller's config
        and will be passed to all controller functions as the first argument. This function should return a new controller
        config. This is a place where you can setup any stateful processes that will be used by the controller. For instance if
        you want to use `go-loop` to handle commands sent to controller, this is a place where you would create the incoming
        channel"
          controller-dispatcher)

(defmulti api
          "Called after `init` and before `start`. This function can expose an object that will be passed as a first argument
          to the API calls. API calls are not wrapped in the transact block automatically."
          controller-dispatcher)

(defmulti terminate
          "Called on controller shutdown (after stop). This function is used to cleanup any resources created in the `init`
        method. This function is used for side-effects and it's return value is ignored."
          controller-dispatcher)

(defmulti start
          "Called once on controller start. This function will receive the controller config map and a previous state
        and should return the initial state synchronously."
          controller-dispatcher)

(defmulti stop
          "Called once when the controller is stopped. This function will receive the controller config map and the current state
        and should return the final state. This allows you to preserve any state (if you need to) while the controller is not
        running. This state will not be visible to any descendants or subscriptions."
          controller-dispatcher)

(defmulti handle
          "Called whenever a command is sent to the controller. It receives the controller config map, the command and the command
        payload as arguments. Be careful when implementing this functions, as it could receive commands (from ancestors) that
        are not handled. Always make sure to check if you handle the command before processing it."
          transacted-controller-dispatcher)

(defmulti derive-state
          "This function will be called whenever the controller's internal state is changed or whenever any of the ancestors updates
        the state. It will receive controller's internal state and a map with the states of ancestors. Use this function to derive
        the state if needed. All descendants and subscriptions will receive the returned value as a payload on the controller's key"
          controller-dispatcher)

(defmethod prep :default [controller]
  controller)

(defmethod api :default [controller])

(defmethod init :default [controller]
  controller)

(defmethod terminate :default [controller])

(defmethod start :default [controller params deps-state prev-state])

(defmethod stop :default [controller params state deps-state])

(defmethod handle :default [controller event payload])

(defmethod handle :keechma.controller/wrap-transaction [controller event payload]
  (transact controller #(handle (assoc controller :keechma/is-transacting true) event payload)))

(defmethod derive-state :default [controller state deps-state]
  state)

(defn dispatch
  ([controller receiver-name event] (dispatch controller receiver-name event nil))
  ([controller receiver-name event payload]
   (let [app (:keechma/app controller)]
     (protocols/-dispatch app receiver-name event payload)
     nil)))

(defn broadcast
  ([controller event] (dispatch controller event nil))
  ([controller event payload]
   (let [app (:keechma/app controller)]
     (protocols/-broadcast app event payload)
     nil)))

(defn transact [controller transaction]
  (let [app (:keechma/app controller)]
    (protocols/-transact app transaction)))

(defn call [controller controller-name api-fn & args]
  (let [app (:keechma/app controller)]
    (protocols/-call app controller-name api-fn args)))

(defn get-api* [controller controller-name]
  (let [app (:keechma/app controller)]
    (protocols/-get-api* app controller-name)))