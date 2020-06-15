(ns keechma.next.toolbox.pipeline
  (:require [cljs.core.async :refer [<! alts! chan put! timeout close!]]
            [promesa.core :as p]
            [medley.core :refer [dissoc-in]])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defprotocol ISideffect
  (call! [this runtime context]))

;(defn prepare-running-pipelines [pipelines]
;  (mapv (fn [p] (select-keys p [:ident :args])) pipelines))

;(defrecord WaitPipelinesSideffect [pipeline-filter]
;  ISideffect
;  (call! [_ {:keys [get-live-pipelines wait-all]} _ _]
;    (let [filtered (pipeline-filter (prepare-running-pipelines (get-live-pipelines)))]
;      (wait-all (map :ident filtered)))))
;
;(defrecord CancelPipelinesSideffect [pipeline-filter]
;  ISideffect
;  (call! [_ {:keys [get-live-pipelines cancel-all]} _ _]
;    (let [filtered (pipeline-filter (prepare-running-pipelines (get-live-pipelines)))]
;      (cancel-all (map :ident filtered)))))

;
;(defn wait-pipelines! [pipeline-filter]
;  (->WaitPipelinesSideffect pipeline-filter))
;
;(defn cancel-pipelines! [pipeline-filter]
;  (->CancelPipelinesSideffect pipeline-filter))

(defn error? [value]
  (instance? js/Error value))

(defn sideffect? [value]
  (satisfies? ISideffect value))

(defn pipeline? [value]
  (let [m (meta value)]
    (::pipeline? m)))

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

(defn execute [ident runtime action context value error]
  (try
    (let [invoke (:invoke runtime)
          result (if error (action value context error) (action value context))
          {:keys [val repr]} result]
      (cond
        (sideffect? val) (call-sideffect val runtime context)
        (pipeline? val) (invoke val value ident)
        (promise? val) (p/then val (fn [val'] (when (sideffect? val') (throw (ex-info (:async-sideffect pipeline-errors) {})) val')))
        :else val))
    (catch :default err
      err)))

(defn get-next-pipeline-state [pipeline block value]



  (let [actions (get pipeline block)
        [action & rest-actions] actions]
    [action (assoc pipeline block rest-actions)]))

(defn pipeline-drained? [pipeline block]
  (not (seq (get pipeline block))))

(defn get-rescue-or-finally-block [pipeline block]
  (cond
    (and (= :begin block) (seq (:rescue pipeline))) :rescue
    (and (= :begin block) (seq (:finally pipeline))) :finally
    (and (= :rescue block) (seq (:finally pipeline))) :finally
    :else nil))

(defn real-value [value prev-value]
  (if (nil? value)
    prev-value
    value))

(defn run-sync-block [runtime props context state]
  (let [{:keys [ident]} props
        {:keys [block prev-value value error pipeline]} state]
    (loop [block block
           pipeline pipeline
           prev-value prev-value
           value value
           error error]
      (let [{:keys [begin rescue finally]} pipeline]
        (cond
          (= ::cancelled value)
          [:result value]

          (promise? value)
          [:promise {:pipeline pipeline :block block :value (p/then value #(real-value % prev-value)) :prev-value prev-value :error error}]

          :else
          (case block
            :begin
            (cond
              (error? value)
              (recur :rescue pipeline prev-value prev-value value)

              (not (seq begin))
              (recur :finally pipeline prev-value value error)

              :else
              (let [[action & rest-actions] begin
                    next-value (execute ident runtime action context value error)]
                (recur :begin (assoc pipeline :begin rest-actions) value (real-value next-value value) error)))

            :rescue
            (cond
              (error? value)
              (recur :finally pipeline prev-value prev-value value)

              (not (seq rescue))
              (recur :finally pipeline prev-value value error)

              :else
              (let [[action & rest-actions] rescue
                    next-value (execute ident runtime action context value error)]
                (recur :rescue (assoc pipeline :rescue rest-actions) value (real-value next-value value) error)))

            :finally
            (cond
              (error? value)
              [:error value]

              (not (seq finally))
              [:result value]

              :else
              (let [[action & rest-actions] finally
                    next-value (execute ident runtime action context value error)]
                (recur :finally (assoc pipeline :finally rest-actions) value (real-value next-value value) error)))))))))

(defn ^:private run-pipeline [pipeline props runtime context value]
  (let [{:keys [get-state transact]} runtime
        {:keys [promise canceller]} props
        [res-type payload] (transact #(run-sync-block runtime props context {:block :begin :value value :pipeline pipeline}))]
    (cond
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
              (let [[next-res-type next-payload] (transact #(run-sync-block runtime props context (assoc state :value value)))]
                (cond
                  (= :result next-res-type) (p/resolve! promise next-payload)
                  (= :error next-res-type) (p/reject! promise next-payload)
                  :else (recur next-payload))))))
        ::async))))

(defn make-pipeline [id pipeline]
  (with-meta (partial run-pipeline pipeline)
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

(defn register-pipeline [pipelines pipeline-name pipeline]
  (assoc pipelines pipeline-name {:pipeline pipeline
                                  :config (update (::config (meta pipeline)) :queue #(or % pipeline-name))}))

(defn register-pipelines [pipelines pipelines-to-register]
  (reduce-kv
   (fn [pipelines' pipeline-name pipeline]
     (register-pipeline pipelines' pipeline-name pipeline))
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

(defn make-runtime
  ([context pipelines] (make-runtime context pipelines {:pipeline/transactor default-transactor}))
  ([context pipelines {:pipeline/keys [transactor] :as opts}]
   (let [pipelines$ (atom (register-pipelines {} pipelines))
         pipelines-state$ (atom {})]

     (add-watch pipelines-state$ :watcher
                (fn [_ _ _ new-value]
                  ;;(println (with-out-str (cljs.pprint/pprint new-value)))
                  ))
     (letfn [(make-api
               ([] (make-api nil))
               ([props]
                (merge
                  {:invoke invoke
                   :cancel cancel
                   :cancel-all cancel-all
                   :wait-all wait-all
                   :has-pipeline? has-pipeline?
                   :shutdown-runtime shutdown-runtime
                   :get-state get-state
                   :get-live-pipelines get-live-pipelines
                   :transact transact
                   :pipelines-state$ pipelines-state$}
                  props)))

             (transact [fn]
               (transactor fn))

             (register
               ([pipeline] (register (get-pipeline-name pipeline) pipeline))
               ([pipeline-name pipeline]
                (reset! pipelines$ (register-pipeline @pipelines$ pipeline-name pipeline))
                pipeline-name))

             (has-pipeline? [name]
               (get @pipelines$ name))

             (shutdown-runtime []
               (let [pipelines @pipelines$
                     live-pipelines (get-live-pipelines)]
                 (doseq [p live-pipelines]
                   (let [[pipeline-name _] (:ident p)
                         config (get-pipeline-config pipelines pipeline-name)]
                     (when (:cancel-on-shutdown config)
                       (cancel (:ident p)))))))

             (get-live-pipelines []
               (filter
                 (fn [s]
                   (contains? #{::running ::idle ::cancelling} (:state s)))
                 (vals (:pipelines @pipelines-state$))))

             (get-state
               ([]
                (get-in @pipelines-state$ [:pipelines]))
               ([ident]
                (get-in @pipelines-state$ [:pipelines ident])))

             (cancel [ident]
               (let [pipelines-state @pipelines-state$
                     pipeline-state (get-in pipelines-state [:pipelines ident])
                     {:keys [cancelled cancelling]} (get-idents-for-cancel pipelines-state ident)
                     canceller (get-in pipelines-state [:pipelines cancelled :props :canceller])]
                 (when (contains? #{::idle ::running} (:state pipeline-state))
                   (when canceller
                     (close! canceller))
                   (cancel-idle-promises! pipelines-state (concat [cancelled] cancelling))
                   (reset! pipelines-state$ (-> pipelines-state
                                                (update-pipelines-state-state ::cancelled [cancelled])
                                                (update-pipelines-state-state ::cancelling cancelling))))))

             (wait-all [idents]
               (let [pipelines-state @pipelines-state$
                     pipeline-promises
                     (map
                       (fn [ident]
                         (get-in pipelines-state [:pipelines ident :props :promise]))
                       idents)]
                 (p/all pipeline-promises)))

             (cancel-all [idents]
               (doseq [ident idents]
                 (cancel ident)))

             (enqueue [ident {{:keys [promise]} :props :keys [owner-ident args] :as state}]
               (let [[pipeline-name _] ident
                     pipelines @pipelines$
                     pipelines-state @pipelines-state$
                     pipeline (get-pipeline pipelines pipeline-name)
                     pipeline-config (get-pipeline-config pipelines pipeline-name)
                     queue-name (get-queue-name pipeline-config args)
                     pipeline-queue (get-queue pipelines-state queue-name)]

                 ;; For pipelines that can be started immediately, we call them and check the return
                 ;; value. If the return value is ::async that means that the pipeline has encountered
                 ;; a promise as a return value from one of the blocks. In other case we know that pipeline
                 ;; consisted only of synchronous functions and we do cleanup immediatelly after.
                 (if (pipeline-can-start-immediately? pipeline-queue pipeline-config)
                   (do
                     (reset! pipelines-state$
                             (cond-> pipelines-state
                               true (assoc-in [:pipelines ident] (assoc state :state ::running))
                               true (add-to-queue queue-name ident)
                               owner-ident (assoc-in [:pipelines owner-ident :owned-ident] ident)))
                     (let [api (make-api {:get-state (partial get-state ident)})
                           result (pipeline (:props state) api context (:args state))]
                       (if (= ::async result)
                         (do
                           (p/finally promise #(finish ident))
                           promise)
                         (do
                           (finish ident)
                           result))))
                   (let [queued-idents-to-cancel (get-queued-idents-to-cancel pipeline-config pipeline-queue)]
                     (p/finally promise #(finish ident))
                     (doseq [ident queued-idents-to-cancel]
                       (cancel ident))
                     (if (= :dropping (get-in pipeline-config [:concurrency :behavior]))
                       (p/resolve! (get-in state [:props :promise]) ::cancelled)
                       (reset! pipelines-state$
                               (cond-> pipelines-state
                                 true (assoc-in [:pipelines ident] state)
                                 true (add-to-queue queue-name ident)
                                 owner-ident (assoc-in [:pipelines owner-ident :owned-ident] ident))))
                     promise))))

             (finish [ident]
               (let [pipelines @pipelines$
                     pipelines-state @pipelines-state$
                     [pipeline-name _] ident
                     pipeline-config (get-pipeline-config pipelines pipeline-name)
                     {:keys [owner-ident args] :as state} (get-in pipelines-state [:pipelines ident])
                     queue-name (get-queue-name pipeline-config args)
                     pipeline-state (get-in pipelines-state [:pipelines ident])]
                 (when pipeline-state
                   (close! (get-in pipeline-state [:props :canceller]))
                   (reset! pipelines-state$
                           (cond-> pipelines-state
                             true (dissoc-in [:pipelines ident])
                             true (remove-from-queue queue-name ident)
                             owner-ident (dissoc-in [:pipelines owner-ident :owned-ident])))
                   (let [pipelines-state @pipelines-state$
                         pipeline-queue (get-queue pipelines-state queue-name)
                         queued-idents-to-start (get-queued-idents-to-start pipeline-config pipeline-queue)]
                     (doseq [ident queued-idents-to-start]
                       (let [api (make-api {:get-state (partial get-state ident)})
                             [pipeline-name _] ident
                             pipeline (get-pipeline pipelines pipeline-name)
                             state (get-in @pipelines-state$ [:pipelines ident])]
                         (swap! pipelines-state$ assoc-in [:pipelines ident :state] ::running)
                         (pipeline (:props state) api context (:args state))))))))

             (invoke
               ([pipeline-name] (invoke pipeline-name nil nil))
               ([pipeline-name args] (invoke pipeline-name args nil))
               ([pipeline-name args owner-ident]
                (if (and (fn? pipeline-name) (::pipeline? (meta pipeline-name)))
                  (invoke (register pipeline-name) args owner-ident)
                  (let [pipeline (get-pipeline @pipelines$ pipeline-name)
                        ident [pipeline-name (keyword (gensym :pipeline/instance))]]
                    (if pipeline
                      (if-let [existing-promise (existing @pipelines$ @pipelines-state$ pipeline-name args)]
                        existing-promise
                        (let [promise (p/deferred)
                              canceller (chan)
                              state {:state ::idle
                                     :ident ident
                                     :owner-ident owner-ident
                                     :args args
                                     :props {:ident ident
                                             :promise promise
                                             :canceller canceller}}]
                          (when ^boolean goog.DEBUG
                            (p/catch promise #(js/console.error %)))
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

(defn pswap! [& args]
  (apply swap! args)
  nil)

(defn preset! [& args]
  (apply reset! args)
  nil)