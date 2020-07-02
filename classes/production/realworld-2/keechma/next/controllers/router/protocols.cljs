(ns keechma.next.controllers.router.protocols)

(defprotocol IRouterApi
  (get-url [this params]))