(ns keechma.next.controllers.pipelines
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.protocols :as protocols]
            [keechma.next.toolbox.pipeline :refer [start! stop! invoke has-pipeline?]]
            [clojure.set :as set]
            [keechma.next.toolbox.pipeline.runtime :as ppr]))

(derive ::controller :keechma/controller)

(defn get-throwable-pipelines-for-queue [state detached-idents {:keys [queue]}]
  (->> queue
       (map
         (fn [ident]
           (let [instance (get-in state [:instances ident])]
             (when-not (or (= ::ppr/waiting-children (:state instance))
                           (contains? detached-idents ident))
               {:ident ident
                :deferred-result (get-in instance [:props :deferred-result])}))))
       (remove nil?)
       vec))

(defn get-detached-idents [{:keys [instances] :as state}]
  (let [detached-idents (->> instances
                             (filter (fn [[_ v]] (get-in v [:resumable :config :is-detached])))
                             (map first))]
    (reduce
      (fn [acc detached-ident]
        (set/union acc (set (ppr/get-ident-and-descendant-idents state detached-ident))))
      #{}
      detached-idents)))

(defn make-watcher [{:keys [meta-state*]}]
  (fn [_ _ _ new-value]
    (let [{:keys [queues]} new-value
          detached-idents (get-detached-idents new-value)
          grouped (reduce-kv (fn [m k v] (assoc m k (get-throwable-pipelines-for-queue new-value detached-idents v))) {} queues)]
      (swap! meta-state* assoc ::state grouped))))

(defn get-promise
  ([meta-state pipeline]
   (:deferred-result (last (get-in meta-state [::state pipeline]))))
  ([meta-state pipeline args]
   (:deferred-result (last (filter #(= args (:args %)) (get-in meta-state [::state pipeline]))))))

(defn throw-promise!
  ([meta-state pipeline]
   (when-let [p (get-promise meta-state pipeline)]
     (throw p)))
  ([meta-state pipeline args]
   (when-let [p (get-promise meta-state pipeline args)]
     (throw p))))

(defmethod ctrl/init ::controller [ctrl]
  (let [pipelines (:keechma/pipelines ctrl)
        pipelines' (if (fn? pipelines) (pipelines ctrl) pipelines)
        app (:keechma/app ctrl)]
    (if pipelines
      (let [runtime* (atom nil)
            ctrl' (assoc ctrl ::runtime* runtime*)
            opts {:transactor (partial protocols/-transact app)
                  :watcher (make-watcher ctrl')}
            runtime (start! ctrl' pipelines' opts)]
        (reset! runtime* runtime)
        ctrl')
      ctrl)))

(defmethod ctrl/handle ::controller [ctrl cmd payload]
  (when-let [runtime* (::runtime* ctrl)]
    (let [runtime @runtime*]
      (when (has-pipeline? runtime cmd)
        (invoke runtime cmd payload)))))

(defmethod ctrl/terminate ::controller [ctrl]
  (when-let [runtime* (get-in ctrl [::runtime*])]
    (let [runtime @runtime*]
      (stop! runtime))))

(defn register [ctrl pipelines]
  (assoc ctrl :keechma/pipelines pipelines))