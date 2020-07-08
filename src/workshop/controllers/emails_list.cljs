(ns workshop.controllers.emails-list
  (:require [keechma.next.controller :as ctrl]
            [workshop.domain.emails :refer [logged-in-email]]))

(derive :emails-list :keechma/controller)

(defmethod ctrl/derive-state :emails-list [_ _ {:keys [router emails]}]
  (let [top-emails (filterv #(not (:thread-id %)) emails)]
    (case (:page router)
      "inbox" top-emails
      "sent" (filterv #(= logged-in-email (:from %)) emails)
      "important" (filterv :isImportant top-emails)
      nil)))