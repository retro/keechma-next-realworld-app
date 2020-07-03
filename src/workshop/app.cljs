(ns workshop.app
  (:require ["react-dom" :as rdom]))

(def app
  {:keechma.subscriptions/batcher rdom/unstable_batchedUpdates
   :keechma/controllers {}
   :keechma/apps {}})