(ns keechma.next.toolbox.pipeline
  (:require [cljs.core.async :refer [<! alts! chan put! timeout close!]]
            [promesa.core :as p]
            [medley.core :refer [dissoc-in]]
            [keechma.next.toolbox.pipeline.runtime :as runtime])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]
                   [keechma.next.toolbox.pipeline :refer [pipeline!]]))

(def make-pipeline runtime/make-pipeline)

(defn set-queue-name
  [pipeline queue]
  (assoc-in pipeline [:config :queue-name] queue))

(defn use-existing
  [pipeline]
  (assoc-in pipeline [:config :use-existing] true))

(defn restartable
  ([pipeline] (restartable pipeline 1))
  ([pipeline max-concurrency]
   (assoc-in pipeline [:config :concurrency] {:behavior :restartable :max max-concurrency})))

(defn enqueued
  ([pipeline] (enqueued pipeline 1))
  ([pipeline max-concurrency]
   (assoc-in pipeline [:config :concurrency] {:behavior :enqueued :max max-concurrency})))

(defn dropping
  ([pipeline] (dropping pipeline 1))
  ([pipeline max-concurrency]
   (assoc-in pipeline [:config :concurrency] {:behavior :dropping :max max-concurrency})))

(defn keep-latest
  ([pipeline] (keep-latest pipeline 1))
  ([pipeline max-concurrency]
   (assoc-in pipeline [:config :concurrency] {:behavior :keep-latest :max max-concurrency})))

(defn cancel-on-shutdown
  ([pipeline] (cancel-on-shutdown pipeline true))
  ([pipeline should-cancel]
   (assoc-in pipeline [:config :cancel-on-shutdown] should-cancel)))

#_(defn detach [pipeline]
  (with-meta
    (fn [_ runtime _ value]
      (let [{:keys [get-state invoke]} runtime
            {:keys [owner-ident]} (get-state)]
        (invoke pipeline value owner-ident true)
        nil))
    {::pipeline? true}))

#_(defn mute [pipeline]
  (pipeline! [value _]
    (let [value' value]
      (pipeline! [_ _]
        pipeline
        value'))))

(defn pswap! [& args]
  (apply swap! args)
  nil)

(defn preset! [& args]
  (apply reset! args)
  nil)