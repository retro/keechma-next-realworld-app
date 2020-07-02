(ns keechma.next.helix.lib
  #?(:clj (:require [helix.core :as helix]))
  #?(:cljs (:require-macros [keechma.next.helix.lib])))

#?(:clj
   (defmacro defnc [type params & body]
     (let [opts? (map? (first body)) ;; whether an opts map was passed in
           opts (if opts?
                  (first body)
                  {})
           body (if opts?
                  (rest body)
                  body)
           ;; feature flags to enable by default
           default-opts {:helix/features {:fast-refresh false}}]
       `(helix.core/defnc ~type ~params
          ;; we use `merge` here to allow indidivual consumers to override feature
          ;; flags in special cases
          ~(merge default-opts opts)
          ~@body))))
