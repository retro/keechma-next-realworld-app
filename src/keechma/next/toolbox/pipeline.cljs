(ns keechma.next.toolbox.pipeline
  (:require [cljs.core.async :refer [<! alts! chan put! timeout close!]]
            [promesa.core :as p]
            [medley.core :refer [dissoc-in]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]
                   [keechma.next.toolbox.pipeline :refer [pipeline!]]))

(def ^:dynamic *pipeline-depth* 0)

(defprotocol ISideffect
  (call! [this runtime context]))

(defrecord ResumableState [ident args state tail])

(defn resumable? [val]
  (instance? ResumableState val))

(defn fn->pipeline [pipeline-fn]
  (with-meta pipeline-fn {::pipeline? true}))

(defn error? [value]
  (instance? js/Error value))

(defn sideffect? [value]
  (satisfies? ISideffect value))

(defn pipeline? [value]
  (let [m (meta value)]
    (::pipeline? m)))

(defn in-pipeline? []
  (pos? *pipeline-depth*))

(def promise? p/promise?)

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

(def pipeline-errors
  {:async-sideffect "Returning sideffects from promises is not permitted. It is possible that application state was modified in the meantime"})

(defn call-sideffect [val runtime context]
  (let [res (call! val runtime context)]
    (if (promise? res)
      (->> res (p/map (constantly nil) (constantly nil)))
      nil)))

(defn execute [ident runtime get-interpreter-state action context value error]
  (try
    (let [invoke (:invoke runtime)
          val (if error (action value context error) (action value context))]
      (cond
        (sideffect? val) (call-sideffect val runtime context)
        (pipeline? val) (invoke val value ident false get-interpreter-state)
        (promise? val) (p/then val (fn [val'] (when (sideffect? val') (throw (ex-info (:async-sideffect pipeline-errors) {}))) val'))
        :else val))
    (catch :default err
      err)))

(defn real-value [value prev-value]
  (if (nil? value)
    prev-value
    value))

(declare start-interpreter)

(defn interpreter-state->resumable [stack]
  (reduce
    (fn [acc v]
      (assoc (map->ResumableState v) :tail acc))
    nil
    stack))

(defn interpreter-state->pipeline [interpreter-state]
  (fn->pipeline
    (fn [props runtime context _]
      (start-interpreter (interpreter-state->resumable interpreter-state) props runtime context))))

(defn run-sync-block [runtime props context state tail]
  (binding [*pipeline-depth* (inc *pipeline-depth*)]
    (let [{:keys [get-state resume]} runtime
          {:keys [ident]} props
          {:keys [block prev-value value error pipeline]}
          (if tail
            (let [resumed-value (resume tail ident false (constantly (:tail tail)))]
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
                (let [{:keys [args interpreter-state*]} (get-state)
                      state {:block block :pipeline (update pipeline block rest) :prev-value prev-value :value value :error error}
                      continuation-state {:ident ident :args args :state state}
                      interpreter-state @interpreter-state*]
                  (into [continuation-state] interpreter-state)))]
          (cond
            (= ::cancelled value)
            [:result value]

            (resumable? value)
            [:resumable-state value]

            (promise? value)
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
                  (recur :finally (assoc pipeline :finally rest-actions) value (real-value next-value value) error))))))))))

(defn run-sync-block-until-no-resumable-state [runtime props context state tail]
  (let [{:keys [transact]} runtime
        [res-type payload] (transact #(run-sync-block runtime props context state tail))]
    (if (= :resumable-state res-type)
      (if (:is-root props)
       (recur runtime props context (:state payload) (:tail payload))
       [res-type payload])
      [res-type payload])))

(defn start-interpreter [interpreter-state props runtime context]
  (let [{:keys [state tail]} interpreter-state
        {:keys [get-state transact]} runtime
        {:keys [promise canceller]} props
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
                    (= ::cancelled (:state (get-state))))
              (p/resolve! promise ::cancelled)
              (let [[next-res-type next-payload] (transact #(run-sync-block-until-no-resumable-state runtime props context (assoc state :value value) nil))]
                (cond
                  (= :resumable-state res-type) payload
                  (= :result next-res-type) (p/resolve! promise next-payload)
                  (= :error next-res-type) (p/reject! promise next-payload)
                  :else (recur next-payload))))))
        ::async))))

(defn ^:private run-pipeline [pipeline props runtime context value]
  (if (fn? pipeline)
    (pipeline props runtime context value)
    (let [interpreter-state (if (resumable? value) value {:state {:block :begin :value value :pipeline pipeline}})]
      (start-interpreter interpreter-state props runtime context))))

(defn make-pipeline [id pipeline]
  (with-meta pipeline                                       ;;(partial run-pipeline pipeline)
             {::id        id
              ::pipeline? true
              ::config    {:concurrency        {:max js/Infinity}
                           :cancel-on-shutdown true}}))

(defn get-pipeline-name [pipeline]
  (let [pmeta (meta pipeline)]
    (keyword (str "pipeline-" (hash pmeta)))))

(defn get-idents-for-cancel [pipelines-state ident]
  (loop [ident ident
         idents {:cancelling []}]
    (let [s (get-in pipelines-state [:pipelines ident])]
      (if-let [owned-ident (:owned-ident s)]
        (recur owned-ident (update idents :cancelling conj ident))
        (assoc idents :cancelled ident)))))

(defn update-pipelines-state-state [pipelines-state state idents]
  (reduce
   (fn [pipelines-state' ident]
     (if-let [current-state (get-in pipelines-state' [:pipelines ident])]
       (assoc-in pipelines-state' [:pipelines ident] (assoc current-state :state state))
       pipelines-state'))
   pipelines-state
   idents))

(defn register-pipeline [pipelines pipeline-name pipeline is-temp]
  (assoc pipelines pipeline-name {:pipeline pipeline
                                  :is-temp is-temp
                                  :config (update (::config (meta pipeline)) :queue #(or % pipeline-name))}))

(defn register-pipelines [pipelines pipelines-to-register]
  (reduce-kv
   (fn [pipelines' pipeline-name pipeline]
     (register-pipeline pipelines' pipeline-name pipeline false))
   pipelines
   pipelines-to-register))

(defn get-pipeline [pipelines pipeline-name]
  (get-in pipelines [pipeline-name :pipeline]))

(defn get-pipeline-config [pipelines pipeline-name]
  (get-in pipelines [pipeline-name :config]))

(defn get-queue [pipelines-state queue-name]
  (->> (get-in pipelines-state [:queues queue-name])
       (mapv #(get-in pipelines-state [:pipelines %]))))

(defn add-to-queue [pipelines-state queue-name ident]
  (update-in pipelines-state [:queues queue-name] #(vec (conj % ident))))

(defn remove-from-queue [pipelines-state queue-name ident]
  (let [q (get-in pipelines-state [:queues queue-name])]
    (assoc-in pipelines-state [:queues queue-name] (filterv #(not= ident %) q))))

(defn pipeline-can-start-immediately? [queue config]
  (let [max-concurrency (or (get-in config [:concurrency :max]) js/Infinity)
        enqueued (filter #(contains? #{::idle ::running} (:state %)) queue)]
    (> max-concurrency (count enqueued))))

(defn get-queued-idents-to-cancel [pipeline-config pipeline-queue]
  (let [max-concurrency (get-in pipeline-config [:concurrency :max])
        free-slots (dec max-concurrency)
        behavior (get-in pipeline-config [:concurrency :behavior])]
    (case behavior
      :restartable
      (let [cancellable (filterv #(contains? #{::running ::idle} (:state %)) pipeline-queue)]
        (map :ident (take (- (count cancellable) free-slots) cancellable)))
      :keep-latest
      (map :ident (filterv #(= ::idle (:state %)) pipeline-queue))
      [])))

(defn get-queued-idents-to-start [pipeline-config pipeline-queue]
  (let [max-concurrency (get-in pipeline-config [:concurrency :max])
        idle            (filterv #(= ::idle (:state %)) pipeline-queue)
        running         (filterv #(= ::running (:state %)) pipeline-queue)]
    (map :ident (take (- max-concurrency (count running)) idle))))

(defn cancel-idle-promises! [pipelines-state idents]
  (doseq [ident idents]
    (let [state   (get-in pipelines-state [:pipelines ident :state])
          promise (get-in pipelines-state [:pipelines ident :props :promise])]
      (when (= ::idle state)
        (p/resolve! promise ::cancelled)))))

(defn get-queue-name [pipeline-config args]
  (let [queue (:queue pipeline-config)]
    (if (fn? queue)
      (queue args)
      queue)))

(defn existing [pipelines pipelines-state pipeline-name current-args]
  (let [pp-config (get-pipeline-config pipelines pipeline-name)]
    (when (:use-existing pp-config)
      (let [queue-name  (get-queue-name pp-config current-args)
            pp-queue    (get-queue pipelines-state queue-name)
            existing-pp (->> pp-queue
                             (filter (fn [q]
                                       (and (= current-args (:args q))
                                            (contains? #{::idle ::running} (:state q))
                                            (= queue-name (first (:ident q))))))
                             first)]
        (when existing-pp
          (get-in existing-pp [:props :promise]))))))

(defn default-transactor [transaction]
  (transaction))

(def default-opts
  {:transactor default-transactor
   :watcher (fn [& args])
   :error-reporter (if goog.DEBUG js/console.error identity)})

(defn make-runtime
  ([context pipelines] (make-runtime context pipelines default-opts))
  ([context pipelines opts]
   (let [{:keys [transactor watcher error-reporter]} (merge default-opts opts)
         pipelines* (atom (register-pipelines {} pipelines))
         pipelines-state* (atom {})]

     (add-watch pipelines-state* ::watcher watcher)

     (letfn [(make-api
               ([] (make-api nil))
               ([props]
                (merge
                  {:invoke invoke
                   :resume resume
                   :cancel cancel
                   :cancel-all cancel-all
                   :wait-all wait-all
                   :has-pipeline? has-pipeline?
                   :shutdown-runtime shutdown-runtime
                   :get-state get-state
                   :get-live-pipelines get-live-pipelines
                   :transact transact
                   :pipelines-state* pipelines-state*}
                  props)))

             (transact [fn]
               (transactor fn))

             (register
               ([pipeline] (register (get-pipeline-name pipeline) pipeline false))
               ([pipeline-name pipeline] (register pipeline-name pipeline false))
               ([pipeline-name pipeline is-temp]
                (reset! pipelines* (register-pipeline @pipelines* pipeline-name pipeline is-temp))
                pipeline-name))

             (has-pipeline? [name]
               (get @pipelines* name))

             (shutdown-runtime []
               (let [pipelines @pipelines*
                     live-pipelines (get-live-pipelines)]
                 (doseq [p live-pipelines]
                   (let [[pipeline-name _] (:ident p)
                         config (get-pipeline-config pipelines pipeline-name)]
                     (when (:cancel-on-shutdown config)
                       (cancel (:ident p)))))
                 (remove-watch pipelines-state* ::watcher)))

             (get-live-pipelines []
               (filter
                 (fn [s]
                   (contains? #{::running ::idle ::cancelling} (:state s)))
                 (vals (:pipelines @pipelines-state*))))

             (get-state
               ([]
                (get-in @pipelines-state* [:pipelines]))
               ([ident]
                (get-in @pipelines-state* [:pipelines ident])))

             (get-detached-pipelines-idents [idents]
               (let [pipelines-state @pipelines-state*]
                 (map first (filter (fn [[_ v]] (contains? idents (:detached-owner-ident v))) (:pipelines pipelines-state)))))

             (cancel [ident]
               (let [pipelines-state @pipelines-state*
                     pipeline-state (get-in pipelines-state [:pipelines ident])
                     {:keys [cancelled cancelling]} (get-idents-for-cancel pipelines-state ident)
                     detached-pipelines-idents (get-detached-pipelines-idents (set (conj cancelling cancelled)))
                     canceller (get-in pipelines-state [:pipelines cancelled :props :canceller])]
                 (doseq [detached-pipeline-ident detached-pipelines-idents]
                   (cancel detached-pipeline-ident))
                 (when (contains? #{::idle ::running} (:state pipeline-state))
                   (when canceller
                     (close! canceller))
                   (cancel-idle-promises! pipelines-state (concat [cancelled] cancelling))
                   (reset! pipelines-state* (-> pipelines-state
                                                (update-pipelines-state-state ::cancelled [cancelled])
                                                (update-pipelines-state-state ::cancelling cancelling))))))

             (wait-all [idents]
               (let [pipelines-state @pipelines-state*
                     pipeline-promises
                     (map
                       (fn [ident]
                         (get-in pipelines-state [:pipelines ident :props :promise]))
                       idents)]
                 (p/all pipeline-promises)))

             (cancel-all [idents]
               (doseq [ident idents]
                 (cancel ident)))

             (get-initial-value [state]
               (or (:continuation-state state) (:args state)))

             (enqueue [ident {{:keys [promise]} :props :keys [owner-ident args] :as state}]
               (let [[pipeline-name _] ident
                     pipelines @pipelines*
                     pipelines-state @pipelines-state*
                     pipeline (get-pipeline pipelines pipeline-name)
                     pipeline-config (get-pipeline-config pipelines pipeline-name)
                     queue-name (get-queue-name pipeline-config args)
                     pipeline-queue (get-queue pipelines-state queue-name)]

                 ;;(println (keys state))

                 ;; For pipelines that can be started immediately, we call them and check the return
                 ;; value. If the return value is ::async that means that the pipeline has encountered
                 ;; a promise as a return value from one of the blocks. In other case we know that pipeline
                 ;; consisted only of synchronous functions and we do cleanup immediatelly after.
                 (if (pipeline-can-start-immediately? pipeline-queue pipeline-config)
                   (do
                     (reset! pipelines-state*
                             (cond-> pipelines-state
                               true (assoc-in [:pipelines ident] (assoc state :state ::running))
                               true (add-to-queue queue-name ident)
                               owner-ident (assoc-in [:pipelines owner-ident :owned-ident] ident)))
                     (let [api (make-api {:get-state (partial get-state ident)})
                           result (try
                                    (run-pipeline pipeline (:props state) api context (get-initial-value state))
                                    (catch :default e
                                      (error-reporter e)))]
                       (if (= ::async result)
                         (do
                           (p/finally promise #(finish ident))
                           promise)
                         (do
                           (p/resolve! promise result)
                           (finish ident)
                           result))))
                   (let [queued-idents-to-cancel (get-queued-idents-to-cancel pipeline-config pipeline-queue)]
                     (p/finally promise #(finish ident))
                     (doseq [ident queued-idents-to-cancel]
                       (cancel ident))
                     (if (= :dropping (get-in pipeline-config [:concurrency :behavior]))
                       (p/resolve! (get-in state [:props :promise]) ::cancelled)
                       (reset! pipelines-state*
                               (cond-> pipelines-state
                                 true (assoc-in [:pipelines ident] state)
                                 true (add-to-queue queue-name ident)
                                 owner-ident (assoc-in [:pipelines owner-ident :owned-ident] ident))))
                     promise))))

             (finish [ident]
               (let [pipelines @pipelines*
                     pipelines-state @pipelines-state*
                     [pipeline-name _] ident
                     pipeline-config (get-pipeline-config pipelines pipeline-name)
                     {:keys [owner-ident args] :as state} (get-in pipelines-state [:pipelines ident])
                     queue-name (get-queue-name pipeline-config args)
                     pipeline-state (get-in pipelines-state [:pipelines ident])]
                 (when pipeline-state
                   (close! (get-in pipeline-state [:props :canceller]))
                   (reset! pipelines-state*
                           (cond-> pipelines-state
                             true (dissoc-in [:pipelines ident])
                             true (remove-from-queue queue-name ident)
                             owner-ident (dissoc-in [:pipelines owner-ident :owned-ident])))
                   (let [pipelines-state @pipelines-state*
                         pipeline-queue (get-queue pipelines-state queue-name)
                         queued-idents-to-start (get-queued-idents-to-start pipeline-config pipeline-queue)]
                     (doseq [ident queued-idents-to-start]
                       (let [api (make-api {:get-state (partial get-state ident)})
                             [pipeline-name _] ident
                             pipeline (get-pipeline pipelines pipeline-name)
                             state (get-in @pipelines-state* [:pipelines ident])]
                         (swap! pipelines-state* assoc-in [:pipelines ident :state] ::running)
                         (try
                           (let [res (run-pipeline pipeline (:props state) api context (get-initial-value state))]
                             (when (and (not (promise? res))
                                        (not= ::async res))
                               (finish ident))
                             (if (= ::async res)
                               (get-in @pipelines-state* [:pipelines ident :props :promise])
                               res))
                           (catch :default e
                             (error-reporter e)))))))))

             ;; TODO: cleanup and rewrite
             ;; TODO: check why we need registration of pipelines
             (resume
               ([continuation-state] (resume continuation-state nil false (initial-continuation-state)))
               ([continuation-state owner-ident] (resume continuation-state owner-ident false (initial-continuation-state)))
               ([continuation-state owner-ident is-detached] (resume continuation-state owner-ident is-detached (initial-continuation-state)))
               ([continuation-state owner-ident is-detached get-interpreter-state]
                (let [{:keys [ident args]} continuation-state
                      [pipeline-name _] ident
                      pipeline (get-pipeline @pipelines* pipeline-name)
                      ident' [pipeline-name (keyword (gensym :pipeline/resumed-instance))]
                      is-detached-child (when owner-ident (get-in @pipelines-state* [:pipelines owner-ident :is-detached]))]

                  (let [promise (p/deferred)
                        canceller (chan)
                        state {:state ::idle
                               :ident ident'
                               :is-detached (or is-detached is-detached-child)
                               :owner-ident (when-not is-detached owner-ident)
                               :detached-owner-ident (when is-detached owner-ident)
                               :interpreter-state* (reify IDeref (-deref [_] (get-interpreter-state)))
                               :continuation-state (assoc continuation-state :ident ident')
                               :args args
                               :props {:ident ident
                                       :is-root (or (and is-detached owner-ident) (not owner-ident))
                                       :promise promise
                                       :canceller canceller}}]
                    (when ^boolean goog.DEBUG
                      (p/catch promise error-reporter))
                    (enqueue ident state))
                  #_(if pipeline

                    (do
                      ;;(register pipeline-name)
                      ;;     (recur continuation-state owner-ident is-detached get-interpreter-state)
                      )))))

             (initial-continuation-state [] (constantly []))

             (invoke
               ([pipeline-name] (invoke pipeline-name nil nil false (initial-continuation-state)))
               ([pipeline-name args] (invoke pipeline-name args nil false (initial-continuation-state)))
               ([pipeline-name args owner-ident] (invoke pipeline-name args owner-ident false (initial-continuation-state)))
               ([pipeline-name args owner-ident is-detached] (invoke pipeline-name args owner-ident is-detached (initial-continuation-state)))
               ([pipeline-name args owner-ident is-detached get-interpreter-state]
                (if (::pipeline? (meta pipeline-name))
                  (invoke (register pipeline-name) args owner-ident is-detached get-interpreter-state)
                  (let [pipeline (get-pipeline @pipelines* pipeline-name)
                        ident [pipeline-name (keyword (gensym :pipeline/instance))]]
                    (if pipeline
                      (if-let [existing-promise (existing @pipelines* @pipelines-state* pipeline-name args)]
                        existing-promise
                        (let [is-detached-child (when owner-ident (get-in @pipelines-state* [:pipelines owner-ident :is-detached]))
                              promise (p/deferred)
                              canceller (chan)
                              state {:state ::idle
                                     :ident ident
                                     :is-detached (or is-detached is-detached-child)
                                     :owner-ident (when-not is-detached owner-ident)
                                     :detached-owner-ident (when is-detached owner-ident)
                                     :interpreter-state* (reify IDeref (-deref [_] (get-interpreter-state)))
                                     :args args
                                     :props {:ident ident
                                             :is-root (or (and is-detached owner-ident) (not owner-ident))
                                             :promise promise
                                             :canceller canceller}}]
                          (when ^boolean goog.DEBUG
                            (p/catch promise error-reporter))
                          (enqueue ident state)))
                      (throw (ex-info (str "Pipeline " pipeline-name " is not registered with runtime") {})))))))]
       (make-api)))))

(defn set-queue
  [pipeline queue]
  (vary-meta pipeline assoc-in [::config :queue] queue))

(defn use-existing
  [pipeline]
  (vary-meta pipeline assoc-in [::config :use-existing] true))

(defn restartable
  ([pipeline] (restartable pipeline 1))
  ([pipeline max-concurrency]
   (vary-meta pipeline assoc-in [::config :concurrency] {:behavior :restartable :max max-concurrency})))

(def exclusive restartable)

(defn enqueued
  ([pipeline] (enqueued pipeline 1))
  ([pipeline max-concurrency]
   (vary-meta pipeline assoc-in [::config :concurrency] {:behavior :enqueued :max max-concurrency})))

(defn dropping
  ([pipeline] (dropping pipeline 1))
  ([pipeline max-concurrency]
   (vary-meta pipeline assoc-in [::config :concurrency] {:behavior :dropping :max max-concurrency})))

(defn keep-latest
  ([pipeline] (keep-latest pipeline 1))
  ([pipeline max-concurrency]
   (vary-meta pipeline assoc-in [::config :concurrency] {:behavior :keep-latest :max max-concurrency})))

(defn cancel-on-shutdown
  ([pipeline] (cancel-on-shutdown pipeline true))
  ([pipeline should-cancel]
   (vary-meta pipeline assoc-in [::config :cancel-on-shutdown] should-cancel)))

(defn detach [pipeline]
  (with-meta
    (fn [_ runtime _ value]
      (let [{:keys [get-state invoke]} runtime
            {:keys [owner-ident]} (get-state)]
        (invoke pipeline value owner-ident true)
        nil))
    {::pipeline? true}))

(defn mute [pipeline]
  (pipeline! [value _]
    (let [value' value]
      (pipeline! [_ _]
        pipeline
        value'))))

(defn pswap! [& args]
  (apply swap! args)
  nil)

(defn preset! [& args]
  (apply reset! args)
  nil)