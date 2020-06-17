(ns keechma.next.controllers.pipelines
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.toolbox.pipeline :refer [make-runtime]]))

(derive ::controller :keechma/controller)

(defn make-watcher [{:keys [meta-state$ keechma.controller/name]}]
  (fn [_ _ _ new-value]
    (let [{:keys [queues pipelines]} new-value
          grouped (reduce-kv
                     (fn [m k v]
                       (assoc m k (map (fn [p] {:promise (get-in pipelines [p :props :promise]) :args (get-in pipelines [p :args])}) v)))
                     {}
                     queues)]
      (swap! meta-state$ assoc ::state grouped))))

(defn get-promise
  ([meta-state pipeline]
   (:promise (first (get-in meta-state [::state pipeline]))))
  ([meta-state pipeline args]
   (:promise (first (filter #(= args (:args %)) (get-in meta-state [::state pipeline]))))))

(defn throw-promise!
  ([meta-state pipeline]
   (when-let [p (get-promise meta-state pipeline)]
     (throw p)))
  ([meta-state pipeline args]
   (when-let [p (get-promise meta-state pipeline args)]
     (throw p))))

(defmethod ctrl/init ::controller [ctrl]
  (let [pipelines (:keechma/pipelines ctrl)]
    (if pipelines
      (let [opts {:transactor (get-in ctrl [:keechma.controller/api :keechma/app :transact])
                  :watcher (make-watcher ctrl)}
            runtime (make-runtime ctrl pipelines opts)]
        (assoc ctrl ::runtime runtime))
      ctrl)))

(defmethod ctrl/receive ::controller [ctrl cmd payload]
  (when-let [runtime (::runtime ctrl)]
    (let [{:keys [has-pipeline? invoke]} runtime]
      (when (has-pipeline? cmd)
        (invoke cmd payload)))))

(defmethod ctrl/terminate ::controller [ctrl]
  (when-let [shutdown-runtime (get-in ctrl [::runtime :shutdown-runtime])]
    (shutdown-runtime)))

(defn register [ctrl pipelines]
  (assoc ctrl :keechma/pipelines pipelines))