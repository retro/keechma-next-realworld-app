(ns keechma.next.core
  (:require
    [keechma.next.controller :as ctrl]
    [keechma.next.graph :refer [subgraph-reachable-from subgraph-reachable-from-set]]
    [medley.core :refer [dissoc-in]]
    [keechma.next.util :refer [shallow-identical? get-lowest-common-ancestor-for-paths]]
    [keechma.next.spec]
    [cljs.spec.alpha :as s]
    [com.stuartsierra.dependency :as dep]
    [clojure.set :as set]
    [keechma.next.protocols :as protocols :refer [IAppInstance IRootAppInstance ITransact]]))

(declare reconcile-from!)
(declare -send!)
(declare reconcile-after-transaction!)
(declare reconcile-initial!)

(def ^:dynamic *transaction-depth* 0)

(defn transacting? []
  (pos? *transaction-depth*))

#_(defn pp [& args]
    (apply println (map (fn [a] (with-out-str (cljs.pprint/pprint a))) args)))

(defn keechma-ex-info
  ([message anomaly] (keechma-ex-info message anomaly {}))
  ([message anomaly props]
   (ex-info message (assoc props
                      :keechma.anomalies/category anomaly
                      :keechma.anomalies/message message))))

(defn default-batcher [batched-fn]
  (batched-fn))

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

(deftype DepsState [app-state* controller-name]
  IDeref
  (-deref [_] (get-controller-derived-deps-state @app-state* controller-name)))

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

(defn unsubscribe! [app-state* controller-name sub-id]
  (swap! app-state* dissoc-in [:subscriptions controller-name sub-id]))

(defn unsubscribe-meta! [app-state* controller-name sub-id]
  (swap! app-state* dissoc-in [:subscriptions-meta controller-name sub-id]))

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

(defn get-app-store-path [app-path]
  (vec (interpose :apps (concat [:apps] app-path))))

(defn get-apps-store-path [app-path]
  (vec (concat (get-app-store-path app-path) [:apps])))

(defn register-controllers-app-index [app-state path controllers]
  (let [app-controllers (->> controllers (map (fn [[k _]] [k path])) (into {}))]
    (-> app-state
        (assoc-in [:app->controllers-index path] (set (keys controllers)))
        (update :controller->app-index #(merge % app-controllers)))))

(defn deregister-controllers-app-index [app-state path controller-names]
  (-> app-state
      (dissoc-in [:app->controllers-index path])
      (update :controller->app-index #(apply dissoc % controller-names))))

(defn register-app [app-state {:keys [path controllers] :as app-ctx}]
  (-> app-state
      (assoc-in (get-app-store-path path) app-ctx)
      (update :controllers merge controllers)
      (register-controllers-app-index path controllers)))

(defn deregister-app [app-state path]
  (let [app-store-path (get-app-store-path path)
        app-ctx (get-in app-state app-store-path)
        controller-names (set (keys (:controllers app-ctx)))
        remove-controllers #(apply dissoc % controller-names)]
    (-> app-state
        (dissoc-in app-store-path)
        (update :controllers remove-controllers)
        (update :app-db remove-controllers)
        (deregister-controllers-app-index path controller-names)
        (update-in [:transaction :dirty] set/difference controller-names))))

(defn make-ctx
  [app {:keys [ancestor-controllers apps-context] :as initial-ctx}]
  (let [apps (prepare-apps (:keechma/apps app))
        controllers (prepare-controllers (:keechma/controllers app))
        apps-context' (dissoc app :keechma/controllers :keechma/apps)
        visible-controllers (set/union ancestor-controllers (set (keys controllers)))
        controllers-graph (build-controllers-graph controllers)]
    (merge
      initial-ctx
      {:keechma/apps apps
       :controllers controllers
       :apps-context (merge apps-context apps-context')
       :ancestor-controllers (or ancestor-controllers #{})
       :visible-controllers visible-controllers
       :controllers-graph controllers-graph
       :apps nil})))

(defn assoc-empty-transaction [app-state]
  (assoc app-state :transaction {:dirty #{} :dirty-meta #{}}))

(defn transact [app-state* transaction]
  (let [res (binding [*transaction-depth* (inc *transaction-depth*)] (transaction))]
    (reconcile-after-transaction! app-state*)
    res))

(defn make-controller-instance [app-state* controller-name params]
  (let [controller (get-in @app-state* [:controllers controller-name])
        state* (atom nil)
        meta-state* (atom nil)
        id (keyword (gensym 'controller-instance-))]

    (assoc controller
      :keechma.controller/name controller-name
      :keechma.controller/params params
      :keechma.controller/id id
      :keechma/app (reify
                     IAppInstance
                     (-send! [_ controller-name event] (-send! app-state* controller-name event nil))
                     (-send! [_ controller-name event payload] (-send! app-state* controller-name event payload))
                     ITransact
                     (-transact [_ transaction]
                       (transact app-state* transaction)))
      :meta-state* meta-state*
      :state* state*
      :deps-state* (->DepsState app-state* controller-name))))

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

(defn batched-notify-subscriptions-meta
  ([{:keys [batcher] :as app-state}]
   (batcher #(notify-subscriptions-meta app-state)))
  ([{:keys [batcher] :as app-state} dirty-meta]
   (batcher #(notify-subscriptions-meta app-state dirty-meta))))

(defn batched-notify-subscriptions [{:keys [batcher] :as app-state}]
  (batcher #(notify-subscriptions app-state)))

(defn sync-controller->app-db! [app-state* controller-name]
  (let [app-state @app-state*
        app-db (:app-db app-state)
        controller (get app-db controller-name)
        instance (:instance controller)
        state (deref (:state* instance))
        deps-state (get-controller-derived-deps-state app-state controller-name)
        new-app-db
        (-> app-db
            (dissoc-in [controller-name :commands-buffer])
            (assoc-in [controller-name :state] state)
            (assoc-in [controller-name :derived-state]
                      (ctrl/derive-state instance state deps-state)))]
    (swap! app-state* assoc :app-db new-app-db)))

(defn sync-controller-meta->app-db! [app-state* controller-name]
  (let [app-state @app-state*
        meta-state* (get-in app-state [:app-db controller-name :instance :meta-state*])]
    (swap! app-state* assoc-in [:app-db controller-name :meta-state] @meta-state*)))

(defn transaction-dirty-data? [app-state controller-name]
  (let [dirty-data (get-in app-state [:transaction :dirty-data])]
    (contains? dirty-data controller-name)))

(defn transaction-pending? [app-state controller-name]
  (let [{:keys [pending reconciled]} (:transaction app-state)]
    (contains? (set/difference pending reconciled) controller-name)))

(defn transaction-mark-dirty! [app-state* controller-name]
  (swap! app-state* update-in [:transaction :dirty] conj controller-name))

(defn transaction-unmark-dirty! [app-state* controller-name]
  (swap! app-state* update-in [:transaction :dirty] disj controller-name))

(defn transaction-mark-dirty-meta! [app-state* controller-name]
  (swap! app-state* update-in [:transaction :dirty-meta] conj controller-name))

(defn on-controller-state-change [app-state* controller-name]
  (sync-controller->app-db! app-state* controller-name)
  (if (transacting?)
    (transaction-mark-dirty! app-state* controller-name)
    (reconcile-from! app-state* controller-name)))

(defn on-controller-meta-state-change [app-state* controller-name]
  (sync-controller-meta->app-db! app-state* controller-name)
  (if (transacting?)
    (transaction-mark-dirty-meta! app-state* controller-name)
    (batched-notify-subscriptions-meta @app-state* #{controller-name})))

(defn get-controller-instance [app-state controller-name]
  (get-in app-state [:app-db controller-name :instance]))

(defn get-sorted-controllers-for-app [app-state path]
  (let [{:keys [controllers controllers-graph]} (get-in app-state (get-app-store-path path))
        nodeset (dep/nodes controllers-graph)
        sorted-controllers (filterv #(contains? controllers %) (dep/topo-sort controllers-graph))
        isolated-controllers (->> (keys controllers)
                                  (filter #(not (contains? nodeset %))))]
    (concat isolated-controllers sorted-controllers)))

;; TODO: Handle command buffering
(defn -send!
  ([app-state* controller-name cmd] (-send! app-state* controller-name cmd))
  ([app-state* controller-name cmd payload]
   (transact app-state* #(ctrl/receive (get-controller-instance @app-state* controller-name) cmd payload))))

(defn controller-start! [app-state* controller-name params]
  (swap! app-state* assoc-in [:app-db controller-name] {:params params :phase :initializing :commands-buffer []})
  (let [config (make-controller-instance app-state* controller-name params)
        instance (ctrl/init config)]
    (swap! app-state* assoc-in [:app-db controller-name :instance] instance)
    (let [state* (:state* instance)
          meta-state* (:meta-state* instance)
          prev-state (get-in @app-state* [:app-db controller-name :state])
          deps-state (get-controller-derived-deps-state @app-state* controller-name)
          state (ctrl/start instance params deps-state prev-state)]

      (reset! state* state)
      (swap! app-state* update-in [:app-db controller-name] #(merge % {:state state :phase :starting}))
      (-send! app-state* controller-name :keechma.on/start params)
      (swap! app-state* assoc-in [:app-db controller-name :phase] :running)
      (doseq [[cmd payload] (get-in @app-state* [:app-db controller-name :commands-buffer])]
        (-send! app-state* controller-name cmd payload))
      (sync-controller->app-db! app-state* controller-name)
      (sync-controller-meta->app-db! app-state* controller-name)
      (add-watch meta-state* :keechma/app #(on-controller-meta-state-change app-state* controller-name))
      (add-watch state* :keechma/app #(on-controller-state-change app-state* controller-name)))))

(defn controller-stop! [app-state* controller-name]
  (let [instance (get-in @app-state* [:app-db controller-name :instance])
        params (:keechma.controller/params instance)
        state* (:state* instance)]
    (swap! app-state* assoc-in [:app-db controller-name :phase] :stopping)
    (remove-watch state* :keechma/app)
    (remove-watch (:meta-state* instance) :keechma/app)
    (-send! app-state* controller-name :keechma.on/stop nil)
    (let [deps-state (get-controller-derived-deps-state @app-state* controller-name)
          state (ctrl/stop controller-name params @state* deps-state)]
      (reset! state* state)
      (swap! app-state* assoc-in [:app-db controller-name] {:state state}))
    (ctrl/terminate instance)))

(defn controller-on-deps-change! [app-state* controller-name]
  (let [app-state @app-state*
        controller (get-in app-state [:controllers controller-name])
        prev-deps-state (get-in app-state [:app-db controller-name :prev-deps-state])
        deps-state (get-controller-derived-deps-state @app-state* controller-name)
        instance (get-in app-state [:app-db controller-name :instance])]
    (when (and (seq (:keechma.controller/deps controller))
               (not (shallow-identical? prev-deps-state deps-state)))
      (swap! app-state* assoc-in [:app-db controller-name :prev-deps-state] deps-state)
      (-send! app-state* controller-name :keechma.on/deps-change deps-state)
      (let [state (-> instance :state* deref)
            derived-state (ctrl/derive-state instance state deps-state)]
        (swap! app-state* assoc-in [:app-db controller-name :derived-state] derived-state)))))

(defn reconcile-controller-lifecycle-state! [app-state* controller-name]
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
  (let [params (get-params @app-state* controller-name)
        actions (determine-actions (get-in @app-state* [:app-db controller-name :params]) params)]
    (when (contains? actions :stop)
      (controller-stop! app-state* controller-name))
    (when (contains? actions :start)
      (controller-start! app-state* controller-name params))
    (when (contains? actions :deps-change)
      (controller-on-deps-change! app-state* controller-name))))

(defn reconcile-controller-factory! [app-state* controller-name]
  (let [app-state @app-state*
        controllers (:controllers app-state)
        app-db (:app-db app-state)
        config (get controllers controller-name)
        prev-produced-keys (get-in app-db [controller-name :produced-keys])
        produced
        (->> (get-factory-produced @app-state* controller-name)
             (reduce
               (fn [acc [k produced-config]]
                 (let [produced-controller-name (conj controller-name k)
                       conformed-produced-config
                       (s/conform :keechma.controller.factory/produced produced-config)
                       factory-produced-deps (:keechma.controller/deps conformed-produced-config)]
                   #_(when (and ^boolean goog.DEBUG (seq factory-produced-deps))
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
        running-keys (->> @app-state*
                          :app-db
                          (filter (fn [[k v]] (and (contains? prev-produced-keys k) (= :running (:phase v)))))
                          (map first)
                          set)
        to-remove (set/difference running-keys produced-keys)]

    (doseq [controller-name to-remove]
      (controller-stop! app-state* controller-name))

    (swap! app-state*
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
          (reconcile-controller-lifecycle-state! app-state* controller-name)
          (recur rest-produced-keys))))))

(defn reconcile-controllers! [app-state* to-reconcile]
  (loop [to-reconcile' to-reconcile]
    (when (seq to-reconcile')
      (let [[current & rest-to-reconcile] to-reconcile'
            controller-variant (get-in @app-state* [:controllers current :keechma.controller/variant])]
        (if (= :factory controller-variant)
          (reconcile-controller-factory! app-state* current)
          (reconcile-controller-lifecycle-state! app-state* current))
        (transaction-unmark-dirty! app-state* current)
        (recur rest-to-reconcile)))))

(defn stop-app! [app-state* path]
  (let [app-state @app-state*
        app-ctx (get-in app-state (get-app-store-path path))
        apps (:apps app-ctx)
        controllers (:controllers app-ctx)]
    (doseq [[app-name _] apps]
      (stop-app! app-state* (conj path app-name)))
    (doseq [[controller-name _] controllers]
      (let [controller (get-in @app-state* [:app-db controller-name])]
        (when (:instance controller)
          (controller-stop! app-state* controller-name))))
    (swap! app-state* deregister-app path)))

(defn reconcile-app! [app-state* path dirty]
  (let [app-state @app-state*
        {:keys [controllers-graph]} (get-in app-state (get-app-store-path path))
        subgraph (subgraph-reachable-from-set controllers-graph dirty)
        to-reconcile (dep/topo-sort subgraph)]
    (reconcile-controllers! app-state* to-reconcile)
    (let [app-state @app-state*
          app-ctx (get-in app-state (get-app-store-path path))
          apps-definitions (:keechma/apps app-ctx)
          apps-by-should-run
          (reduce-kv
            (fn [acc app-name app]
              (let [should-run? (:keechma.app/should-run? app)
                    deps (get-derived-deps-state app-state (:controllers app-state) (:keechma.app/deps app))]
                (update acc (boolean (should-run? deps)) conj app-name)))
            {true #{} false #{}}
            apps-definitions)]
      (doseq [app-name (get apps-by-should-run false)]
        (stop-app! app-state* (conj path app-name)))
      (doseq [app-name (get apps-by-should-run true)]
        (let [app-state @app-state*
              path (conj path app-name)
              child-app-ctx (get-in app-state (get-app-store-path path))]
          (if (:running? child-app-ctx)
            (reconcile-app! app-state* path (set/union dirty (get-in app-state [:transaction :dirty])))
            (do
              (swap! app-state* register-app (make-ctx (get apps-definitions app-name) (merge app-ctx {:path path :running? true})))
              (reconcile-initial! app-state* path))))))))

(defn reconcile-after-transaction! [app-state*]
  (when (not (transacting?))
    (let [app-state @app-state*
          transaction (:transaction app-state)
          dirty (:dirty transaction)
          dirty-meta (:dirty-meta transaction)]
      (cond
        (seq dirty)
        (let [controller-apps (set (map #(get-in app-state [:controller->app-index %]) dirty))
              lca-path (get-lowest-common-ancestor-for-paths controller-apps)]
          (swap! app-state* assoc-empty-transaction)
          (binding [*transaction-depth* (inc *transaction-depth*)]
            (reconcile-app! app-state* lca-path dirty))
          (let [app-state @app-state*
                dirty (get-in app-state [:transaction :dirty])]
            (if (seq dirty)
              (recur app-state*)
              (batched-notify-subscriptions @app-state*))))

        (seq dirty-meta)
        (do
          (swap! app-state* assoc-empty-transaction)
          (batched-notify-subscriptions-meta @app-state* dirty-meta))))))

(defn reconcile-from! [app-state* controller-name]
  (let [app-state @app-state*
        path (get-in app-state [:controller->app-index controller-name])]
    (binding [*transaction-depth* (inc *transaction-depth*)]
      (reconcile-app! app-state* path #{controller-name}))
    (reconcile-after-transaction! app-state*)))

(defn reconcile-initial!
  ([app-state* path] (reconcile-initial! app-state* path 0))
  ([app-state* path depth]
   (let [app-state @app-state*
         to-reconcile (get-sorted-controllers-for-app app-state path)
         app-ctx (get-in app-state (get-app-store-path path))
         apps-definitions (:keechma/apps app-ctx)]
     (binding [*transaction-depth* (inc *transaction-depth*)]
       (reconcile-controllers! app-state* to-reconcile)
       (doseq [[app-name app] apps-definitions]
         (let [app-state @app-state*
               should-run? (:keechma.app/should-run? app)
               deps (get-derived-deps-state app-state (:controllers app-state) (:keechma.app/deps app))]
           (when (should-run? deps)
             (let [path (conj path app-name)
                   app-ctx (make-ctx app (merge app-ctx {:path path :running? true}))]
               (swap! app-state* register-app app-ctx)
               (reconcile-initial! app-state* path (inc depth)))))))
     (when (zero? depth)
       (reconcile-after-transaction! app-state*)))))

(defn start! [app]
  (let [app-id (str (gensym 'app-id))
        app' (s/conform :keechma/app app)
        batcher (or (:keechma.subscriptions/batcher app) default-batcher)
        ctx (make-ctx app' {:path [] :running? true})
        app-state* (atom (-> {:batcher batcher}
                             (assoc-empty-transaction)
                             (register-app ctx)))]

    (reconcile-initial! app-state* [])

    (reify
      IAppInstance
      (-send! [_ controller-name event]
        (-send! app-state* controller-name event nil))
      (-send! [_ controller-name event payload]
        (-send! app-state* controller-name event payload))
      IRootAppInstance
      ;;TODO: Implement stop
      (-stop! [_])
      (-get-batcher [_]
        batcher)
      (-subscribe! [_ controller-name sub-fn]
        (let [sub-id (keyword (gensym 'sub-id-))]
          (swap! app-state* assoc-in [:subscriptions controller-name sub-id] sub-fn)
          (partial unsubscribe! app-state* controller-name sub-id)))
      (-subscribe-meta! [_ controller-name sub-fn]
        (let [sub-id (keyword (gensym 'sub-meta-id-))]
          (swap! app-state* assoc-in [:subscriptions-meta controller-name sub-id] sub-fn)
          (partial unsubscribe-meta! app-state* controller-name sub-id)))
      (-get-derived-state [_]
        (->> @app-state*
             :app-db
             (map (fn [[k v]]
                    (when
                      (and (not (and (vector? k) (= 1 (count k))))
                           (:instance v))
                      [k (:derived-state v)])))
             (filter identity)
             (into {})))
      (-get-derived-state [_ controller-name]
        (get-in @app-state* [:app-db controller-name :derived-state]))
      (-get-meta-state [_ controller-name]
        (get-in @app-state* [:app-db controller-name :meta-state]))
      (-get-id [_] app-id))))

(defn make-app-proxy [proxied-fn]
  (fn [& args]
    (apply proxied-fn args)))

(def send! (make-app-proxy protocols/-send!))
(def stop! (make-app-proxy protocols/-stop!))
(def subscribe! (make-app-proxy protocols/-subscribe!))
(def subscribe-meta! (make-app-proxy protocols/-subscribe-meta!))
(def get-derived-state (make-app-proxy protocols/-get-derived-state))
(def get-meta-state (make-app-proxy protocols/-get-meta-state))
(def get-batcher (make-app-proxy protocols/-get-batcher))
(def get-id (make-app-proxy protocols/-get-id))