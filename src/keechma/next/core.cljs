(ns keechma.next.core
  (:require
    [keechma.next.controller :as ctrl]
    [keechma.next.graph :refer [subgraph-reachable-from subgraph-reachable-from-set]]
    [medley.core :refer [dissoc-in]]
    [keechma.next.util :refer [shallow-identical?]]
    [keechma.next.spec]
    [cljs.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [clojure.set :as set]))

(def ^:dynamic *transaction-depth* -1)
(def ^:dynamic *reconciling* #{})

(defn transaction? []
  (> *transaction-depth* -1))

(defn reconciling? []
  (not-empty *reconciling*))

(defn keechma-ex-info
  ([message anomaly] (keechma-ex-info message anomaly {}))
  ([message anomaly props]
   (ex-info message (assoc props
                      :keechma.anomalies/category anomaly
                      :keechma.anomalies/message message))))

(defn build-controllers-graph [controllers]
  (reduce
    (fn [acc [k c]]
      (reduce
        (fn [acc' d]
          (dep/depend acc' k d))
        acc
        (:keechma.controller/deps c)))
    (dep/graph)
    controllers))

(defn determine-actions [old-params new-params]
  (cond
    (and (not old-params) new-params)
    #{:start}

    (and old-params (not new-params))
    #{:stop}

    (and old-params new-params (not= old-params new-params))
    #{:stop :start}

    (and old-params new-params (= new-params old-params))
    #{:deps-change}

    :else nil))

(defn register-controllers [app-state app-path controllers]
  (let [controllers' (->> controllers
                          (map (fn [[k v]] [k (assoc v :keechma.app/path app-path)]))
                          (into {}))]
    ;; TODO: ensure we're not overwriting anything
    (update app-state :controllers #(merge % controllers'))))

(defn default-batcher [b] (b))

(defn get-app-store-path [app-path]
  (vec (interpose :keechma/apps (concat [:apps] app-path))))

(defn get-apps-store-path [app-path]
  (vec (concat (get-app-store-path app-path) [:keechma/apps])))

(defn get-derived-deps-state [app-state controllers deps]
  (let [app-db (:app-db app-state)]
    (when (seq deps)
      (-> (reduce
            (fn [acc dep-controller-name]
              (if (= :factory (get-in controllers [dep-controller-name :keechma.controller/variant]))
                (reduce
                  (fn [acc' dep-controller-name']
                    (if (contains? #{:starting :running} (get-in app-db [dep-controller-name' :phase]))
                      (assoc! acc' dep-controller-name' (get-in app-db [dep-controller-name' :derived-state]))
                      acc'))
                  acc
                  (get-in app-db [dep-controller-name :produced-keys]))
                (if (contains? #{:starting :running} (get-in app-db [dep-controller-name :phase]))
                  (assoc! acc dep-controller-name (get-in app-db [dep-controller-name :derived-state]))
                  acc)))
            (transient {})
            deps)
          persistent!))))

(defn get-controller-derived-deps-state [app-state controller-name]
  (let [controllers (:controllers app-state)
        controller (get controllers controller-name)
        controller-name' (or (get controller :keechma.controller/factory) controller-name)
        deps (get-in controllers [controller-name' :keechma.controller/deps])
        res (get-derived-deps-state app-state controllers deps)]
    res))

(deftype DepsState [app-state$ controller-name]
  IDeref
  (-deref [_] (get-controller-derived-deps-state @app-state$ controller-name)))

(defn get-params [app-state controller-name]
  (let [controller (get-in app-state [:controllers controller-name])
        params (:keechma.controller/params controller)]
    (if (= :static (:keechma.controller.params/variant controller))
      params
      (params (get-controller-derived-deps-state app-state controller-name)))))

(defn get-factory-produced [app-state controller-name]
  (let [controller (get-in app-state [:controllers controller-name])
        produce (:keechma.controller.factory/produce controller)]
    (produce (get-controller-derived-deps-state app-state controller-name))))

(defn controller-with-deps-state [app-state controller]
  (assoc controller :keechma.controller/deps (get-controller-derived-deps-state app-state (:keechma.controller/name controller))))

(defn valid-sender-id? [app-state controller]
  (let [controller-name (:keechma.controller/name controller)
        controller-id (:keechma.controller/id controller)]
    (= controller-id (get-in app-state [:app-db controller-name :instance :keechma.controller/id]))))

(defn make-controller-send [app-state$ controllers-graph send!]
  (fn [controller receiver-name command payload]
    (let [predecessors (dep/transitive-dependencies controllers-graph (:keechma.controller/name controller))]
      (when (and (contains? (set predecessors) receiver-name)
                 (valid-sender-id? @app-state$ controller))
        (send! receiver-name command payload)))))

(defn make-controller-send-self [app-state$ send!]
  (fn [controller command payload]
    (when (valid-sender-id? @app-state$ controller)
      (send! (:keechma.controller/name controller) command payload))))

(defn make-controller-broadcast [app-state$ controllers-graph send!]
  (fn [controller command payload]
    (when (valid-sender-id? @app-state$ controller)
      (let [successors (dep/transitive-dependents controllers-graph (:keechma.controller/name controller))]
        (doseq [receiver successors]
          (send! receiver command payload))))))

(defn make-controller-instance [app-state$ controller-name params controllers-graph {:keys [send! transact]}]
  (let [controller (get-in @app-state$ [:controllers controller-name])
        state$ (atom nil)
        meta-state$ (atom nil)
        id (keyword (gensym 'controller-instance-))]

    (assoc controller
      :keechma.controller/name controller-name
      :keechma.controller/params params
      :keechma.controller/id id
      :keechma.controller/api {:keechma/app {:send (make-controller-send app-state$ controllers-graph send!)
                                             :send-self (make-controller-send-self app-state$ send!)
                                             :broadcast (make-controller-broadcast app-state$ controllers-graph send!)
                                             :transact transact}}
      :meta-state$ meta-state$
      :state$ state$
      :deps-state$ (->DepsState app-state$ controller-name))))

(defn get-derived-state
  ([app-state]
   (->> app-state
        :app-db
        (map (fn [[k v]]
               (when
                 (and (not (and (vector? k) (= 1 (count k))))
                      (:instance v))
                 [k (:derived-state v)])))
        (filter identity)
        (into {})))
  ([app-state controller-name]
   (get-in app-state [:app-db controller-name :derived-state])))

(defn get-meta-state [app-state controller-name]
  (get-in app-state [:app-db controller-name :meta-state]))

(defn notify-subscriptions-meta
  ([app-state] (notify-subscriptions-meta app-state nil))
  ([app-state dirty-meta]
   (let [app-db (:app-db app-state)
         subscriptions-meta (:subscriptions-meta app-state)
         selected-subscriptions-meta (if dirty-meta
                                       (select-keys subscriptions-meta dirty-meta)
                                       subscriptions-meta)]
     (doseq [[controller-key subs] selected-subscriptions-meta]
       (let [meta-state (get-in app-db [controller-key :meta-state])]
         (doseq [sub (vals subs)]
           (sub meta-state)))))))

(defn notify-subscriptions [app-state]
  (let [app-db (:app-db app-state)
        subscriptions (:subscriptions app-state)]
    (doseq [[controller-key subs] subscriptions]
      (let [derived-state (get-in app-db [controller-key :derived-state])]
        (doseq [sub (vals subs)]
          (sub derived-state))))
    (notify-subscriptions-meta app-state)))

(defn sync-controller->app-db! [app-state$ controller-name]
  (let [app-state @app-state$
        app-db (:app-db app-state)
        controller (get app-db controller-name)
        instance (:instance controller)
        state (deref (:state$ instance))
        deps-state (get-controller-derived-deps-state app-state controller-name)
        new-app-db
        (-> app-db
            (dissoc-in [controller-name :commands-buffer])
            (assoc-in [controller-name :state] state)
            (assoc-in [controller-name :derived-state]
                      (ctrl/derive-state instance state deps-state)))]
    (swap! app-state$ assoc :app-db new-app-db)))

(defn sync-controller-meta->app-db! [app-state$ controller-name]
  (let [app-state @app-state$
        meta-state$ (get-in app-state [:app-db controller-name :instance :meta-state$])]
    (swap! app-state$ assoc-in [:app-db controller-name :meta-state] @meta-state$)))

(defn mark-dirty! [app-state$ controller-name]
  (swap! app-state$ update :transaction #(conj % controller-name)))

(defn mark-dirty-meta! [app-state$ controller-name]
  (swap! app-state$ update :transaction-meta #(conj % controller-name)))

(defn unsubscribe! [app-state$ controller-name sub-id]
  (swap! app-state$ dissoc-in [:subscriptions controller-name sub-id]))

(defn subscribe! [app-state$ controller-name sub-fn]
  (let [sub-id (keyword (gensym 'sub-id-))]
    (swap! app-state$ assoc-in [:subscriptions controller-name sub-id] sub-fn)
    (partial unsubscribe! app-state$ controller-name sub-id)))

(defn unsubscribe-meta! [app-state$ controller-name sub-id]
  (swap! app-state$ dissoc-in [:subscriptions-meta controller-name sub-id]))

(defn subscribe-meta! [app-state$ controller-name sub-fn]
  (let [sub-id (keyword (gensym 'sub-id-))]
    (swap! app-state$ assoc-in [:subscriptions-meta controller-name sub-id] sub-fn)
    (partial unsubscribe-meta! app-state$ controller-name sub-id)))

(defn validate-factory-produced-deps! [controller-name factory-deps factory-produced-deps]
  (when (not (set/subset? factory-produced-deps factory-deps))
    (throw (keechma-ex-info "Produced controller can only define deps that are a subset of factory's deps"
                            {:controller controller-name
                             :factory-deps factory-deps
                             :factory-produced-deps factory-produced-deps
                             :invalid-deps (set/difference factory-produced-deps factory-deps)}))))

;; TODO: validate deps - controllers can't depend on non-existing controller
;; TODO: allow deps remapping -> [:foo] or [{:target :source}] e.g. [{:foo :bar}]

(defn prepare-controllers [controllers]
  (->> controllers
       (map (fn [[k v]] [k (ctrl/prep v)]))
       (into {})))

(defn prepare-apps [apps]
  (->> apps
       (map (fn [[k v]] [k (update v :keechma.app/deps set)]))
       (into {})))

(defn make-app-ctx
  ([app] (make-app-ctx app {:path [] :ancestor-controllers {}}))
  ([app {:keys [ancestor-controllers] :as initial-ctx}]
   (let [apps (prepare-apps (:keechma/apps app))
         controllers (prepare-controllers (:keechma/controllers app))
         apps-context (dissoc app :keechma/controllers :keechma/apps)
         visible-controllers (merge ancestor-controllers controllers)
         controllers-graph (build-controllers-graph controllers)]
     (merge initial-ctx
            {:apps apps
             :controllers controllers
             :apps-context apps-context
             :visible-controllers visible-controllers
             :controllers-graph controllers-graph}))))

(declare reconcile-after-transaction!)

(defn send!
  ([ctx app-state$ controller-name command] (send! ctx app-state$ controller-name command nil))
  ([ctx app-state$ controller-name command payload]
   (let [app-state @app-state$
         app-path (:path ctx)
         ctrl-app-path (get-in app-state [:controllers controller-name :keechma.app/path])]
     (if (get-in app-state [:controllers controller-name])
       ; If send comes to the app that is not owner of the controller that should receive the message
       ; we find that controller's owner app and use its send. This will usually happen when someone sends
       ; a message to the controller that is owned by a nested app, but the public send! api is exposing
       ; main app's send!
       (if (not= app-path ctrl-app-path)
         (let [ctrl-app-send! (get-in app-state (concat (get-app-store-path ctrl-app-path) [:api :send!]))]
           (ctrl-app-send! controller-name command payload))
         (let [result
               (binding [*transaction-depth* (inc *transaction-depth*)]
                 (let [app-db (:app-db app-state)
                       instance (get-in app-db [controller-name :instance])
                       phase (get-in app-db [controller-name :phase])]
                   (cond
                     (= :starting phase)
                     (do (swap! app-state$ update-in [:app-db controller-name :commands-buffer] #(vec (conj % [command payload])))
                         nil)

                     (= :running phase)
                     (ctrl/receive (controller-with-deps-state @app-state$ instance) command payload)

                     :else nil)))]
           (reconcile-after-transaction! ctx app-state$)
           result))
       (throw (keechma-ex-info "Controller doesn't exist" {:keechma.controller/name controller-name}))))))

(defn transact [ctx app-state$ transaction-fn]
  (let [result (binding [*transaction-depth* (inc *transaction-depth*)]
                 (transaction-fn))]
    (reconcile-after-transaction! ctx app-state$)
    result))

(defn batched-notify-subscriptions-meta
  ([{:keys [batcher]} app-state$] (batcher #(notify-subscriptions-meta @app-state$)))
  ([{:keys [batcher]} app-state$ dirty-meta] (batcher #(notify-subscriptions-meta @app-state$ dirty-meta))))

(defn batched-notify-subscriptions [{:keys [batcher]} app-state$]
  (batcher #(notify-subscriptions @app-state$)))

(declare reconcile-subgraph-from!)

(defn on-controller-state-change [ctx app-state$ controller-name]
  (sync-controller->app-db! app-state$ controller-name)
  (when-not (contains? *reconciling* controller-name)
    (if (transaction?)
      (mark-dirty! app-state$ controller-name)
      (reconcile-subgraph-from! ctx app-state$ controller-name))))

(defn on-controller-meta-state-change [ctx app-state$ controller-name]
  (sync-controller-meta->app-db! app-state$ controller-name)
  (if (and (not (reconciling?)) (not (transaction?)))
    (batched-notify-subscriptions-meta ctx app-state$ [controller-name])
    (mark-dirty-meta! app-state$ controller-name)))

(defn controller-start! [ctx app-state$ controller-name params]
  (swap! app-state$ assoc-in [:app-db controller-name] {:params params :phase :initializing :commands-buffer []})
  (let [controllers-graph (:controllers-graph ctx)
        config (make-controller-instance app-state$ controller-name params controllers-graph {:send! (partial send! ctx app-state$) :transact (partial transact ctx app-state$)})
        instance (ctrl/init config)]
    (swap! app-state$ assoc-in [:app-db controller-name :instance] instance)
    (let [state$ (:state$ instance)
          meta-state$ (:meta-state$ instance)
          prev-state (get-in @app-state$ [:app-db controller-name :state])
          deps-state (get-controller-derived-deps-state @app-state$ controller-name)
          state (ctrl/start instance params deps-state prev-state)]

      (reset! state$ state)
      (swap! app-state$ update-in [:app-db controller-name] #(merge % {:state state :phase :starting}))
      (send! ctx app-state$ controller-name :keechma.on/start params)
      (swap! app-state$ assoc-in [:app-db controller-name :phase] :running)
      (doseq [[cmd payload] (get-in @app-state$ [:app-db controller-name :commands-buffer])]
        (send! ctx app-state$ controller-name cmd payload))
      (sync-controller->app-db! app-state$ controller-name)
      (sync-controller-meta->app-db! app-state$ controller-name)
      (add-watch meta-state$ :keechma/app #(on-controller-meta-state-change ctx app-state$ controller-name))
      (add-watch state$ :keechma/app #(on-controller-state-change ctx app-state$ controller-name)))))

(defn controller-stop! [_ app-state$ controller-name]
  (let [instance (get-in @app-state$ [:app-db controller-name :instance])
        params (:keechma.controller/params instance)
        state$ (:state$ instance)]
    (swap! app-state$ assoc-in [:app-db controller-name :phase] :stopping)
    (remove-watch state$ :keechma/app)
    (remove-watch (:meta-state$ instance) :keechma/app)
    (ctrl/receive instance :keechma.on/stop nil)
    (let [deps-state (get-controller-derived-deps-state @app-state$ controller-name)
          state (ctrl/stop controller-name params @state$ deps-state)]
      (reset! state$ state)
      (swap! app-state$ assoc-in [:app-db controller-name] {:state state}))
    (ctrl/terminate instance)))

(defn controller-on-deps-change! [ctx app-state$ controller-name]
  (let [app-state @app-state$
        controller (get-in app-state [:controllers controller-name])
        prev-deps-state (get-in app-state [:app-db controller-name :prev-deps-state])
        deps-state (get-controller-derived-deps-state @app-state$ controller-name)
        instance (get-in app-state [:app-db controller-name :instance])]
    (when (and (seq (:keechma.controller/deps controller))
               (not (shallow-identical? prev-deps-state deps-state)))
      (swap! app-state$ assoc-in [:app-db controller-name :prev-deps-state] deps-state)
      (send! ctx app-state$ controller-name :keechma.on/deps-change deps-state)
      (let [state (-> instance :state$ deref)
            derived-state (ctrl/derive-state instance state deps-state)]
        (swap! app-state$ assoc-in [:app-db controller-name :derived-state] derived-state)))))

(defn reconcile-controller-lifecycle-state! [ctx app-state$ controller-name]
  ;; For each controller in the to-reconcile vector do what is needed based on the return value of
  ;; the params function
  ;;
  ;; +-------------+----------------+-----------------+----------------------------------------------------------+
  ;; | Prev Params | Current Params | Prev == Current |                         Actions                          |
  ;; +-------------+----------------+-----------------+----------------------------------------------------------+
  ;; | falsy       | falsy          | -               | Do Nothing                                               |
  ;; | truthy      | falsy          | -               | Stop the current controller instance                     |
  ;; | falsy       | truthy         | -               | Start a new controller instance                          |
  ;; | truthy      | truthy         | false           | Stop the current controller instance and start a new one |
  ;; | truthy      | truthy         | true            | Send :keechma.on/deps-change command                     |
  ;; +-------------+----------------+-----------------+----------------------------------------------------------+
  (let [params (get-params @app-state$ controller-name)
        actions (determine-actions (get-in @app-state$ [:app-db controller-name :params]) params)]
    (when (contains? actions :stop)
      (controller-stop! ctx app-state$ controller-name))
    (when (contains? actions :start)
      (controller-start! ctx app-state$ controller-name params))
    (when (contains? actions :deps-change)
      (controller-on-deps-change! ctx app-state$ controller-name))))

(defn reconcile-controller-factory! [ctx app-state$ controller-name]
  (let [app-state @app-state$
        controllers (:controllers app-state)
        app-db (:app-db app-state)
        config (get controllers controller-name)
        prev-produced-keys (get-in app-db [controller-name :produced-keys])
        produced
        (->> (get-factory-produced @app-state$ controller-name)
             (reduce
               (fn [acc [k produced-config]]
                 (let [produced-controller-name (conj controller-name k)
                       conformed-produced-config
                       (s/conform :keechma.controller.factory/produced produced-config)
                       factory-produced-deps (:keechma.controller/deps conformed-produced-config)]
                   (when (and ^boolean goog.DEBUG (seq factory-produced-deps))
                     (let [expanded-factory-deps
                           (reduce
                             (fn [acc d]
                               (let [dep-produced-keys (get-in app-db [d :produced-keys])]
                                 (if (seq dep-produced-keys)
                                   (set/union acc #{d} dep-produced-keys)
                                   (conj acc d))))
                             #{}
                             (set (:keechma.controller/deps config)))]
                       (validate-factory-produced-deps!
                         produced-controller-name
                         expanded-factory-deps
                         factory-produced-deps)))
                   (assoc!
                     acc
                     (conj controller-name k)
                     (-> (merge config conformed-produced-config)
                         (assoc :keechma.controller/variant :identity
                                :keechma.controller/name produced-controller-name
                                :keechma.controller/factory controller-name)))))
               (transient {}))
             persistent!)
        produced-keys (set (keys produced))
        running-keys (->> @app-state$
                          :app-db
                          (filter (fn [[k v]] (and (contains? prev-produced-keys k) (= :running (:phase v)))))
                          (map first)
                          set)
        to-remove (set/difference running-keys produced-keys)]

    (doseq [controller-name to-remove]
      (controller-stop! ctx app-state$ controller-name))

    (swap! app-state$
           (fn [app-state]
             (let [{:keys [controllers app-db]} app-state
                   app-db' (-> (apply dissoc app-db to-remove)
                               (assoc-in [controller-name :produced-keys] produced-keys))
                   controllers' (-> (apply dissoc controllers to-remove)
                                    (merge produced))]
               (assoc app-state :app-db app-db' :controllers controllers'))))

    (loop [produced-keys' produced-keys]
      (when (seq produced-keys')
        (let [[controller-name & rest-produced-keys] produced-keys']
          (binding [*reconciling* (set/union *reconciling* (set produced-keys'))]
            (reconcile-controller-lifecycle-state! ctx app-state$ controller-name))
          (recur rest-produced-keys))))))

(defn reconcile-controllers! [ctx app-state$ to-reconcile]
  (loop [to-reconcile' to-reconcile]
    (when (seq to-reconcile')
      (let [[current & rest-to-reconcile] to-reconcile'
            controller-variant (get-in @app-state$ [:controllers current :keechma.controller/variant])]
        (binding [*reconciling* (set/union *reconciling* (set to-reconcile'))]
          (if (= :factory controller-variant)
            (reconcile-controller-factory! ctx app-state$ current)
            (reconcile-controller-lifecycle-state! ctx app-state$ current)))
        (recur rest-to-reconcile)))))

(declare start!')

(defn reconcile-apps! [ctx app-state$ reconciled-controllers]
  (let [{:keys [apps controllers]} ctx
        app-path (:path ctx)
        apps-store-path (get-apps-store-path app-path)
        apps-state (get-in @app-state$ apps-store-path)
        apps-running (->> apps-state
                          (filter (fn [[_ v]] (:running? v)))
                          (map first)
                          set)
        apps-by-should-run (reduce
                             (fn [acc v]
                               (let [should-run? (get-in apps [v :keechma.app/should-run?])
                                     derived-deps (get-derived-deps-state @app-state$ controllers (get-in apps [v :keechma.app/deps]))]
                                 (update acc (boolean (should-run? derived-deps)) conj v)))
                             {true #{} false #{}}
                             (keys apps))
        apps-to-stop (set/intersection apps-running (get apps-by-should-run false))
        apps-to-start (set/difference (get apps-by-should-run true) apps-running)
        apps-to-reconcile-controllers (set/intersection apps-running (get apps-by-should-run true))]

    (doseq [app-name apps-to-stop]
      (let [stop! (get-in apps-state [app-name :api :stop!])]
        (stop!)))

    (doseq [app-name apps-to-reconcile-controllers]
      (let [reconcile-controllers-from-parent! (get-in apps-state [app-name :api :reconcile-controllers-from-parent!])]
        (reconcile-controllers-from-parent! reconciled-controllers)))

    (doseq [app-name apps-to-start]
      (let [app' (merge (:apps-context ctx) (get apps app-name))
            app-ctx (make-app-ctx app' (merge ctx {:ancestor-controllers (:visible-controllers ctx) :path (vec (conj app-path app-name))}))
            api (start!' app-ctx app-state$)]
        (swap! app-state$ assoc-in (conj apps-store-path app-name) {:running? true :api api})))))

(defn reconcile-controllers-from-parent! [ctx app-state$ parent-reconciled-controllers]
  (let [controllers-graph (:controllers-graph ctx)
        subgraph (subgraph-reachable-from-set controllers-graph parent-reconciled-controllers)
        to-reconcile (filter #(not (contains? parent-reconciled-controllers %)) (dep/topo-sort subgraph))]
    (reconcile-controllers! ctx app-state$ to-reconcile)
    (reconcile-apps! ctx app-state$ (concat to-reconcile parent-reconciled-controllers))))

(defn reconcile-controllers-and-apps! [ctx app-state$ to-reconcile]
  (reconcile-controllers! ctx app-state$ to-reconcile)
  (reconcile-apps! ctx app-state$ to-reconcile)
  ;; When we are not inside the reconciliation (reconcile-controllers! can be called during an existing
  ;; reconcile-controllers run) we notify the subscriptions. This way we are not causing unneeded
  ;; re-renders in the UI layer
  (when (empty? *reconciling*)
    (batched-notify-subscriptions ctx app-state$)))

(defn reconcile-subgraph-from! [ctx app-state$ controller-name]
  (let [controller (get-in @app-state$ [:controllers controller-name])
        controller-name' (or (:keechma.controller/factory controller) controller-name)
        controllers-graph (:controllers-graph ctx)]
    (when (not (contains? *reconciling* controller-name'))
      (let [to-reconcile (rest (dep/topo-sort (subgraph-reachable-from controllers-graph controller-name')))]
        (reconcile-controllers-and-apps! ctx app-state$ to-reconcile)))))

(defn reconcile-initial! [ctx app-state$]
  (let [{:keys [controllers controllers-graph]} ctx
        nodeset (dep/nodes controllers-graph)
        sorted-controllers (filterv #(contains? controllers %) (dep/topo-sort controllers-graph))
        isolated-controllers (->> (keys controllers)
                                  (filter #(not (contains? nodeset %))))
        to-reconcile (concat isolated-controllers sorted-controllers)]

    (reconcile-controllers! ctx app-state$ to-reconcile)
    (reconcile-apps! ctx app-state$ (concat (keys (:ancestor-controllers ctx)) to-reconcile))
    (when (empty? *reconciling*)
      (batched-notify-subscriptions ctx app-state$))))

(defn reconcile-after-transaction! [ctx app-state$]
  (let [app-path (:path ctx)]
    (when-not (transaction?)
      (if (not= app-path [])
        (let [top-reconcile-after-transaction! (get-in @app-state$ (concat (get-app-store-path []) [:api :reconcile-after-transaction!]))]
          ;; TODO: Figure out this case
          (when top-reconcile-after-transaction! (top-reconcile-after-transaction!)))
        (let [app-state @app-state$
              controllers-graph (:controllers-graph ctx)
              transaction (:transaction app-state)
              transaction-meta (:transaction-meta app-state)
              subgraph (subgraph-reachable-from-set controllers-graph transaction)
              to-reconcile (concat (set/difference transaction (dep/nodes subgraph))
                                   (dep/topo-sort subgraph))]

          (swap! app-state$ assoc :transaction #{} :transaction-meta #{})
          (if (seq to-reconcile)
            (reconcile-controllers-and-apps! ctx app-state$ to-reconcile)
            (when (not (reconciling?))
              (batched-notify-subscriptions-meta ctx app-state$ transaction-meta))))))))

(defn stop! [ctx app-state$]
  (let [app-state @app-state$
        app-path (:path ctx)
        app-db (:app-db app-state)
        controllers (:controllers ctx)
        controller-names (keys controllers)
        apps-state (get-in app-state (get-apps-store-path app-path))]

    (doseq [[_ subapp-state] apps-state]
      (when (:running? subapp-state)
        (let [stop! (get-in subapp-state [:api :stop!])]
          (stop!))))

    (doseq [controller-name controller-names]
      (when (= :running (get-in app-db [controller-name :phase]))
        (controller-stop! ctx app-state$ controller-name)))

    (swap! app-state$
           (fn [app-state]
             (let [cleanup-controllers #(apply dissoc % controller-names)]
               (-> app-state
                   (dissoc-in (get-app-store-path app-path))
                   (update :controllers cleanup-controllers)
                   (update :app-db cleanup-controllers)))))))

;; TODO: Check command buffering
(defn start!' [ctx app-state$]
  (swap! app-state$ register-controllers (:path ctx) (:controllers ctx))

  (reconcile-initial! ctx app-state$)

  ;; Return "runtime" API
  {:send! (partial send! ctx app-state$)
   :stop! (partial stop! ctx app-state$)
   :reconcile-after-transaction! (partial reconcile-after-transaction! ctx app-state$)
   :reconcile-controllers-from-parent! (partial reconcile-controllers-from-parent! ctx app-state$)})


(defn start! [app]
  (let [app-state$ (atom {:transaction #{} :transaction-meta #{}})
        app' (s/conform :keechma/app app)
        batcher (or (:keechma.subscriptions/batcher app) default-batcher)]
    (if (= :cljs.spec.alpha/invalid app')
      (s/explain :keechma/app app)
      (let [api (start!' (make-app-ctx app' {:batcher batcher :path []}) app-state$)]
        (swap! app-state$ update-in (get-app-store-path []) #(merge % {:running? true :api api}))
        (assoc api :subscribe! (partial subscribe! app-state$)
                   :subscribe-meta! (partial subscribe-meta! app-state$)
                   :batcher batcher
                   :get-meta-state (fn [& args] (apply get-meta-state (concat [@app-state$] args)))
                   :get-derived-state (fn [& args] (apply get-derived-state (concat [@app-state$] args))))))))

(s/fdef start!
        :args (s/cat :app :keechma/app)
        :ret :keechma/app-instance)