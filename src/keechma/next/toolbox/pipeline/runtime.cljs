(ns keechma.next.toolbox.pipeline.runtime
  (:require [promesa.core :as p]
            [medley.core :refer [dissoc-in]]
            [cljs.core.async :refer [chan put! <! close! alts!]])
  (:require-macros [cljs.core.async.macros :refer [go-loop go]]))

(def ^:dynamic *pipeline-depth* 0)
(declare invoke-resumable)
(declare start-resumable)

(defprotocol IPipelineRuntime
  (invoke [this pipeline] [this pipeline args] [this pipeline args config])
  (cancel [this ident])
  (cancel-all [this idents])
  (wait [this ident])
  (wait-all [this idents])
  (transact [this transact-fn])
  (stop! [this])
  (report-error [this error])
  (get-pipeline-instance* [this ident])
  (get-state* [this]))

(defprotocol IPipeline
  (->resumable [this pipeline-name value]))

(defrecord Resumable [id ident config args state tail])
(defrecord Pipeline [id pipeline config]
  IPipeline
  (->resumable [_ pipeline-name value]
    (map->Resumable {:id id
                     :ident [(or pipeline-name id) (keyword "pipeline" (gensym 'instance))]
                     :args value
                     :config config
                     :state {:block :begin
                             :pipeline pipeline
                             :value value}})))

(defn make-pipeline [id pipeline]
  (map->Pipeline
    {:id id
     :pipeline pipeline
     :config {:concurrency {:max js/Infinity}
              :cancel-on-shutdown true}}))

(defn in-pipeline? []
  (pos? *pipeline-depth*))

(defn resumable? [val]
  (instance? Resumable val))

(defn fn->pipeline-step [pipeline-fn]
  (with-meta pipeline-fn {::pipeline-step? true}))

(defn error? [value]
  (instance? js/Error value))

(defn pipeline? [val]
  (instance? Pipeline val))

(defn pipeline-step? [value]
  (let [m (meta value)]
    (::pipeline-step? m)))

(defn as-error [value]
  (if (error? value)
    value
    (ex-info "Unknown Error" {:value value})))

(defn promise->chan [promise]
  (let [promise-chan (chan)]
    (->> promise
         (p/map (fn [v]
                  (when v
                    (put! promise-chan v))
                  (close! promise-chan)))
         (p/error (fn [e]
                    (put! promise-chan (as-error e))
                    (close! promise-chan))))
    promise-chan))

(defn interpreter-state->resumable [stack]
  (reduce
    (fn [acc v]
      (assoc (map->Resumable v) :tail acc))
    nil
    stack))

;; TODO: refactor args
(defn execute [ident runtime get-interpreter-state action context value error]
  (try
    (let [val (if error (action value context error) (action value context))]
      (cond
        (and (fn? val) (pipeline-step? val))
        (val runtime context value error {:owner-ident ident :get-interpreter-state get-interpreter-state})

        (pipeline? val)
        (let [resumable (->resumable val nil value)]
             (invoke-resumable runtime resumable {:owner-ident ident :get-interpreter-state get-interpreter-state}))

        :else val))
    (catch :default err
      err)))

(defn real-value [value prev-value]
  (if (nil? value)
    prev-value
    value))

(defn run-sync-block [runtime {:keys [ident]} context state tail]
  (let [pipeline-state* (get-pipeline-instance* runtime ident)
        {:keys [block prev-value value error pipeline]}
        (if tail
          (let [resumed-value (invoke-resumable runtime tail {:owner-ident ident :is-detached false :get-interpreter-state (constantly (:tail tail))})]
            (assoc state :value resumed-value))
          state)]

    (loop [block block
           pipeline pipeline
           prev-value prev-value
           value value
           error error]
      (let [{:keys [begin rescue finally]} pipeline
            get-current-interpreter-state
            (fn []
              (let [{:keys [args interpreter-state*]} @pipeline-state*
                    state {:block block :pipeline (update pipeline block rest) :prev-value prev-value :value value :error error}
                    continuation-state {:ident ident :args args :state state}
                    interpreter-state @interpreter-state*]
                (into [continuation-state] interpreter-state)))]
        (cond
          (= ::cancelled value)
          [:result value]

          (resumable? value)
          [:resumable-state value]

          (p/promise? value)
          [:promise {:pipeline pipeline :block block :value (p/then value #(real-value % prev-value)) :prev-value prev-value :error error}]

          :else
          (case block
            :begin
            (cond
              (error? value)
              (cond
                (seq rescue) (recur :rescue pipeline prev-value prev-value value)
                (seq finally) (recur :finally pipeline prev-value prev-value value)
                :else [:error value])

              (not (seq begin))
              (recur :finally pipeline prev-value value error)

              :else
              (let [[action & rest-actions] begin
                    next-value (execute ident runtime get-current-interpreter-state action context value error)]
                (recur :begin (assoc pipeline :begin rest-actions) value (real-value next-value value) error)))

            :rescue
            (cond
              (error? value)
              (cond
                (seq finally) (recur :finally pipeline prev-value prev-value value)
                :else [:error value])

              (not (seq rescue))
              (recur :finally pipeline prev-value value error)

              :else
              (let [[action & rest-actions] rescue
                    next-value (execute ident runtime get-current-interpreter-state action context value error)]
                (recur :rescue (assoc pipeline :rescue rest-actions) value (real-value next-value value) error)))

            :finally
            (cond
              (error? value)
              [:error value]

              (not (seq finally))
              [:result value]

              :else
              (let [[action & rest-actions] finally
                    next-value (execute ident runtime get-current-interpreter-state action context value error)]
                (recur :finally (assoc pipeline :finally rest-actions) value (real-value next-value value) error)))))))))

(defn run-sync-block-until-no-resumable-state [runtime props context state tail]
  (let [[res-type payload] (transact runtime #(run-sync-block runtime props context state tail))]
    (if (= :resumable-state res-type)
      (if (:is-root props)
        (recur runtime props context (:state payload) (:tail payload))
        [res-type payload])
      [res-type payload])))

(defn start-interpreter [interpreter-state props runtime context]
  (let [{:keys [state tail]} interpreter-state
        {:keys [deferred-result canceller ident]} props
        [res-type payload] (run-sync-block-until-no-resumable-state runtime props context state tail)]
    (cond
      (= :resumable-state res-type) payload
      (= :result res-type) payload
      (= :error res-type) (throw payload)
      :else
      (do
        (go-loop [state payload]
          (let [[value c] (alts! [(promise->chan (:value state)) canceller])]
            (if (or (= canceller c)
                    (= ::cancelled value)
                    (= ::cancelled (:state (get-pipeline-instance* runtime ident))))
              (p/resolve! deferred-result ::cancelled)
              (let [[next-res-type next-payload] (run-sync-block-until-no-resumable-state runtime props context (assoc state :value value) nil)]
                (cond
                  (= :resumable-state res-type) payload
                  (= :result next-res-type) (p/resolve! deferred-result next-payload)
                  (= :error next-res-type) (p/reject! deferred-result next-payload)
                  :else (recur next-payload))))))
        deferred-result))))

(def live-states #{::running ::pending ::waiting-detached})
(def running-states #{::running ::waiting-detached})

(defn process-pipeline [[pipeline-name pipeline]]
  [pipeline-name (update-in pipeline [:config :queue-name] #(or % pipeline-name))])

(defn get-pipeline-instance [state ident]
  (get-in state [:instances ident]))

(defn can-use-existing? [resumable]
  (get-in resumable [:config :use-existing]))

(defn get-queue [state queue-name]
  (get-in state [:queues queue-name]))

(defn get-queue-config [state queue-name]
  (get-in state [:queues queue-name :config]))

(defn get-queue-queue [state queue-name]
  (get-in state [:queues queue-name :queue]))

(defn get-resumable-queue-name [resumable]
  (let [queue-name (or (get-in resumable [:config :queue-name]) (:id resumable))]
    (if (fn? queue-name)
      (queue-name (:args resumable))
      queue-name)))

(defn get-existing [state resumable]
  (let [queue-name (get-resumable-queue-name resumable)]
    (->> (get-queue-queue state queue-name)
         (filter
           (fn [ident]
             (let [instance (get-pipeline-instance state ident)]
               (and (= (get-in instance [:resumable :id]) (:id resumable))
                    (= (get-in instance [:resumable :args]) (:args resumable))
                    (contains? live-states (:state instance))))))
         first
         (get-pipeline-instance state))))

(defn add-to-queue [state resumable]
  (let [ident (:ident resumable)
        queue-name (or (get-resumable-queue-name resumable))
        queue (or (get-queue state queue-name) {:config (get-in resumable [:config :concurrency]) :queue []})]
    (assoc-in state [:queues queue-name] (assoc queue :queue (conj (:queue queue) ident)))))

(defn remove-from-queue [state resumable]
  (let [ident (:ident resumable)
        queue-name (get-resumable-queue-name resumable)
        queue-queue (get-queue-queue state queue-name)]
    (assoc-in state [:queues queue-name :queue] (filterv #(not= ident %) queue-queue))))

(defn can-start-immediately? [state resumable]
  (let [queue-name (get-resumable-queue-name resumable)
        queue-config (get-queue-config state queue-name)
        realized-queue (map #(get-pipeline-instance state %) (get-queue-queue state queue-name))
        max-concurrency (get queue-config :max js/Infinity)
        enqueued (filter #(contains? live-states (:state %)) realized-queue)]
    (> max-concurrency (count enqueued))))

(defn update-instance-state [state resumable instance-state]
  (assoc-in state [:instances (:ident resumable) :state] instance-state))

(defn register-instance
  ([state resumable props] (register-instance state resumable props ::pending))
  ([state resumable props instance-state]
   (-> state
       (add-to-queue resumable)
       (assoc-in [:instances (:ident resumable)] {:state instance-state :resumable resumable :props props}))))

(defn deregister-instance [state resumable]
  (-> state
      (remove-from-queue resumable)
      (dissoc-in [:instances (:ident resumable)])))

(defn queue-assoc-last-result [state resumable result]
  (let [queue-name (get-resumable-queue-name resumable)]
    (assoc-in state [:queues queue-name :last-result] result)))

(defn queue-assoc-last-error [state resumable result]
  (let [queue-name (get-resumable-queue-name resumable)]
    (assoc-in state [:queues queue-name :last-error] result)))

(defn queue-assoc-last [state resumable result]
  (if (not= ::cancelled result)
    (if (error? result)
      (queue-assoc-last-error state resumable result)
      (queue-assoc-last-result state resumable result))
    state))

(defn get-queued-idents-to-cancel [state resumable]
  (let [queue-name (get-resumable-queue-name resumable)
        queue (get-queue state queue-name)
        queue-queue (:queue queue)
        max-concurrency (get-in queue [:config :max])
        behavior (get-in queue [:config :behavior])
        free-slots (dec max-concurrency)]
    (case behavior
      :restartable
      (let [cancellable (filterv #(contains? live-states (:state (get-pipeline-instance state %))) queue-queue)]
        (take (- (count cancellable) free-slots) cancellable))

      :keep-latest
      (filterv #(= ::pending (:state (get-pipeline-instance state %))) queue-queue)

      [])))

(defn get-queued-idents-to-start [state resumable]
  (let [queue-name (get-resumable-queue-name resumable)
        queue (get-queue state queue-name)
        queue-queue (:queue queue)
        max-concurrency (get-in queue [:config :max])
        pending (filterv #(= ::pending (:state (get-pipeline-instance state %))) queue-queue)
        running (filterv #(contains? running-states (:state (get-pipeline-instance state %))) queue-queue)]
    (take (- max-concurrency (count running)) pending)))

(defn finish-resumable [{:keys [state*] :as runtime} {:keys [ident] :as resumable} result]
  (when (get-pipeline-instance @state* ident)
    (swap! state* (fn [state]
                    (-> state
                        (queue-assoc-last resumable result)
                        (deregister-instance resumable))))
    (let [queued-idents-to-start (get-queued-idents-to-start @state* resumable)]
      (doseq [ident queued-idents-to-start]
        (start-resumable runtime (:resumable (get-pipeline-instance @state* ident))))))
  result)

(defn enqueue-resumable [{:keys [state*] :as runtime} resumable props]
  (let [queued-idents-to-cancel (get-queued-idents-to-cancel @state* resumable)]
    (swap! state* (fn [state] (-> state (register-instance resumable props ::pending))))
    (cancel-all runtime queued-idents-to-cancel)
    (:deferred-result props)))

(defn start-resumable [{:keys [state* ctx] :as runtime} {:keys [ident] :as resumable}]
  (swap! state* update-instance-state resumable ::running)
  (let [props (:props (get-pipeline-instance @state* ident))
        deferred-result (:deferred-result props)
        res (try
              (start-interpreter resumable props runtime ctx)
              (catch :default e
                (report-error runtime e)
                e))]
    (if (p/promise? res)
      (->> deferred-result
           (p/map #(finish-resumable runtime resumable %))
           (p/error (fn [error]
                      (report-error runtime error)
                      (finish-resumable runtime resumable error))))
      (do (if (error? res)
            (p/reject! deferred-result res)
            (p/resolve! deferred-result res))
          (finish-resumable runtime resumable res)))))

(defn throw-if-queues-not-matching [state resumable]
  (let [queue-name (get-resumable-queue-name resumable)
        queue-config (get-queue-config state queue-name)
        pipeline-config (get-in resumable [:config :concurrency])]
    (when (and queue-config (not= queue-config pipeline-config))
      (throw (ex-info "Pipeline's queue config is not matching queue's config"
                      {:pipeline (:ident resumable)
                       :queue queue-name
                       :queue-config queue-config
                       :pipeline-config pipeline-config})))))

(defn invoke-resumable [runtime resumable {:keys [owner-ident] :as pipeline-opts}]
  (let [{:keys [ctx state* opts]} runtime
        deferred-result (p/deferred)
        canceller (chan)
        props (merge
                pipeline-opts
                {:canceller canceller
                 :ident (:ident resumable)
                 :is-root (nil? owner-ident)
                 :deferred-result deferred-result})
        state @state*]

    (throw-if-queues-not-matching state resumable)

    (or (when (can-use-existing? resumable)
          (get-in (get-existing state resumable) [:props :deferred-result]))
        (when (can-start-immediately? state resumable)
          (do (swap! state* register-instance resumable props ::running)
              (start-resumable runtime resumable)))
        (when (= :dropping (get-in resumable [:config :concurrency :behavior]))
        ::cancelled)
        (enqueue-resumable runtime resumable props))))


(defrecord PipelineRuntime [ctx state* pipelines opts]
  IPipelineRuntime
  (invoke [this pipeline-name]
    (invoke this pipeline-name nil nil))
  (invoke [this pipeline-name args]
    (invoke this pipeline-name args nil))
  (invoke [this pipeline-name args pipeline-opts]
    (let [pipeline (get-in @state* [:pipelines pipeline-name])]
      (invoke-resumable this (->resumable pipeline pipeline-name args) pipeline-opts)))
  (transact [_ transaction]
    (binding [*pipeline-depth* (inc *pipeline-depth*)]
      (let [{:keys [transactor]} opts]
        (transactor transaction))))
  (report-error [_ error]
    (let [reporter (:error-reporter opts)]
      (reporter error)))
  (get-pipeline-instance* [this ident]
    (reify
      IDeref
      (-deref [_] (get-pipeline-instance @state* ident))))
  (cancel [this ident]
    (let [instance (get-pipeline-instance @state* ident)
          canceller (get-in instance [:props :canceller])
          deferred-result (get-in instance [:props :deferred-result])]
      (swap! state* update-instance-state ::cancelled)
      (close! canceller)
      (p/resolve! deferred-result ::cancelled)
      (finish-resumable this (:resumable instance) ::cancelled)))
  (cancel-all [this idents]
    (doseq [ident idents]
      (cancel this ident)))
  (stop! [this]
    (remove-watch state* ::watcher)
    (let [instances (:instances @state*)
          cancellable-idents
          (->> instances
               (filter (fn [[_ v]] (get-in v [:resumable :config :cancel-on-shutdown])))
               (map first))]
      (cancel-all this cancellable-idents))))

(defn default-transactor [transaction]
  (transaction))

(def default-opts
  {:transactor default-transactor
   :watcher (fn [& args])
   :error-reporter (if goog.DEBUG js/console.error identity)})

(defn start!
  ([ctx] (start! ctx nil nil))
  ([ctx pipelines] (start! ctx pipelines nil))
  ([ctx pipelines opts]
   (let [opts' (merge default-opts opts)
         {:keys [watcher]} opts'
         state* (atom {:pipelines (into {} (map process-pipeline pipelines))})]
     (add-watch state* ::watcher watcher)
     (->PipelineRuntime ctx state* pipelines opts'))))