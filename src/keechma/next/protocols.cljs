(ns keechma.next.protocols)

(defprotocol IAppInstance
  (-send! [this controller-name event] [this controller-name event payload])
  (-call [this controller-name api-fn args]))

(defprotocol IRootAppInstance
  (-stop! [this])
  (-subscribe! [this controller-name sub-fn])
  (-subscribe-meta! [this controller-name sub-fn])
  (-get-derived-state [this] [this controller-name])
  (-get-meta-state [this controller-name])
  (-get-batcher [this])
  (-get-id [this]))

(defprotocol ITransact
  (-transact [this transaction]))