(ns keechma.next.toolbox.util
  (:require [clojure.core.async :refer [chan put! close!]]
            [promesa.core :as p]))

(defn error? [value]
  (instance? js/Error value))

(defn as-error [value]
  (if (error? value) 
    value
    (ex-info "Unknown Error" {:value value})))

(defn promise->chan [promise]
  (let [promise-chan (chan)]
    (->> promise
         (p/map (fn [v]
                  (when v
                    (put! promise-chan v))
                  (close! promise-chan)))
         (p/error (fn [e] 
                    (put! promise-chan (as-error e))
                    (close! promise-chan))))
    promise-chan))
