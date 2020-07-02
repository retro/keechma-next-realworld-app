(ns keechma.next.controllers.dataloader.protocols)

(defprotocol IDataloaderApi
  (req [this loader] [this loader req-opts] [this loader req-opts dataloader-opts])
  (cached [this loader req-opts]))