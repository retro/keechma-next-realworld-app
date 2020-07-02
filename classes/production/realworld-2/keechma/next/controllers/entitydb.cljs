(ns keechma.next.controllers.entitydb
  (:require [entitydb.entitydb :as edb]
            [keechma.next.controllers.entitydb.protocols :as pt]
            [keechma.next.protocols :as keechma-pt]
            [entitydb.query :as q]
            [keechma.next.controllers.entitydb.controller]))

(derive :keechma/entitydb :keechma/controller)

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

(def include q/include)
(def reverse-include q/reverse-include)
(def recur-on q/recur-on)
(def switch q/switch)

(def insert-entity! (keechma-pt/make-api-proxy pt/insert-entity!))
(def insert-many! (keechma-pt/make-api-proxy pt/insert-many!))
(def insert-named! (keechma-pt/make-api-proxy pt/insert-named!))
(def insert-collection! (keechma-pt/make-api-proxy pt/insert-collection!))
(def remove-entity! (keechma-pt/make-api-proxy pt/remove-entity!))
(def remove-named! (keechma-pt/make-api-proxy pt/remove-named!))
(def remove-collection! (keechma-pt/make-api-proxy pt/remove-collection!))