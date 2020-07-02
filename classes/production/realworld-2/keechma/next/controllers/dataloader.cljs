(ns keechma.next.controllers.dataloader
  (:require [keechma.next.controllers.dataloader.controller]
            [keechma.next.protocols :as keechma-pt]
            [keechma.next.controllers.dataloader.protocols :as pt]))

(derive :keechma/dataloader :keechma/controller)

(def req (keechma-pt/make-api-proxy pt/req))
(def cached (keechma-pt/make-api-proxy pt/cached))
