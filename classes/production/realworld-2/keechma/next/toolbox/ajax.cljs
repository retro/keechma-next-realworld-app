(ns keechma.next.toolbox.ajax
  (:require [ajax.core :as ajax]
            [promesa.core :as p]
            [keechma.next.toolbox.protocols :as pt]
            [keechma.next.toolbox.exceptions :refer [aborted-ex-info]]
            [keechma.next.toolbox.util :refer [as-error]]
            [oops.core :refer [ocall oget]]))

(defn promisify [method]
  (fn [url opts]
    (let [deferred-result (p/deferred)
          default-opts {:handler #(p/resolve! deferred-result %)
                        :error-handler #(p/reject! deferred-result (as-error %))}
          req  (method url (merge opts default-opts))]
      (specify! deferred-result
        pt/IAbortable
        (abort! [this]
          (when (oget req :?abort)
            (ocall req :abort))
          (p/reject! this (aborted-ex-info)))))))

(def GET (promisify ajax/GET))
(def HEAD (promisify ajax/HEAD))
(def POST (promisify ajax/POST))
(def PUT (promisify ajax/PUT))
(def DELETE (promisify ajax/DELETE))
(def OPTIONS (promisify ajax/OPTIONS))
(def TRACE (promisify ajax/TRACE))
(def PATCH (promisify ajax/PATCH))
