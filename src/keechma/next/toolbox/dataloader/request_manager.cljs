(ns keechma.next.toolbox.dataloader.request-manager
  (:require [cljs.core.async :refer [chan close! timeout alts! put!]]
            [keechma.next.toolbox.protocols :as pt]
            [promesa.core :as p])
  (:require-macros [cljs.core.async.macros :refer [go-loop]]))

(defprotocol IRequestManager
  (start [this])
  (request [this loader] [this loader params])
  (abort [this loader params promise])
  (evict [this loader] [this loader params]) 
  (shutdown [this]))

(def default-request-options
  {:dataloader/max-age                0
   :dataloader/max-stale              0
   :dataloader/no-store               true
   :dataloader/stale-while-revalidate false
   :dataloader/stale-if-error         false})

(def default-config
  {:request-interval 20
   :evict-interval (* 1000 60)
   :cache-size 1000})

(defn evict-lru [cache cache-size]
  (->> (map identity cache)
       (sort-by #(get-in % [1 :touched-at]))
       reverse 
       (take cache-size)
       (into {})))

(defn get-dataloader-params [params]
  (into {} (filter (fn [[k v]] (and (keyword? k) (= "dataloader" (namespace k))))) params))

(defn remove-dataloader-params [params]
  (into {} (filter (fn [[k v]] (or (not (keyword? k)) (not= "dataloader" (namespace k)))) params)))

(defn get-time-now []
  (js/Math.floor (/ (js/Date.now) 1000)))

(defn stale? [cached params]
  (let [{:dataloader/keys [max-age]}     params
        {:dataloader/keys [resolved-at]} cached
        time-now                         (get-time-now)]
      (> time-now (+ max-age resolved-at))))

(defn cache-hit? [cached params]
  (let [{:dataloader/keys [max-age max-stale no-store stale-while-revalidate]} params
        {:dataloader/keys [resolved-at]}                                       cached
        time-now                                                               (get-time-now)]
    (cond
      (not cached)                                                                false
      no-store                                                                    false
      (< time-now (+ max-age resolved-at))                                        true
      (and (< time-now (+ max-age max-stale resolved-at)) stale-while-revalidate) true
      :else                                                                       false)))

(defn handle-request [inflight pending loader params deferred-result]
  (let [req-ident       [loader (remove-dataloader-params params)] 
        current-inflight (get inflight req-ident)
        current-pending  (get pending req-ident)]
    (cond
      current-inflight {:inflight (assoc inflight req-ident (conj current-inflight deferred-result)) :pending pending}
      current-pending  {:pending (assoc pending req-ident (conj current-pending deferred-result)) :inflight inflight}
      :else            {:pending (assoc pending req-ident #{deferred-result}) :inflight inflight})))

(defn release-request [reqs req-ident promise]
  (if promise
    (let [promises (get reqs req-ident)
          promises' (disj promises promise)]
      (if (seq promises')
        (assoc reqs req-ident promises')
        (dissoc reqs req-ident)))
    (dissoc reqs req-ident)))

(defn cancel-request [inflight pending requests [loader params promise]]
  (let [req-ident [loader (remove-dataloader-params params)]
        inflight' (release-request inflight req-ident promise)
        pending' (release-request pending req-ident promise)
        should-abort (nil? (get inflight' req-ident))
        requests' (if should-abort (dissoc requests req-ident) requests)]
    (when should-abort
      (let [req (get requests req-ident)]
        (when (satisfies? pt/IAbortable req)
          ;; We want to abort after recurring (on next event cycle)
          (js/setTimeout #(pt/abort! req)))))
    {:inflight inflight'
     :pending pending'
     :requests requests'}))

(defrecord RequestManager [config chans cache-store$]
  IRequestManager
  (start [this]
    (let [{:keys [commander evicter-stopper requester-stopper]} chans]
      (go-loop [inflight {}
                pending {}
                requests {}]
        (let [[cmd & args] (<! (:commander chans))]           
          (when cmd
            (case cmd
              :request
              (let [{:keys [inflight pending]} (apply handle-request inflight pending args)] 
                (recur inflight pending requests))
              :kickoff 
              (let [requests'
                    (reduce-kv 
                     (fn [acc [loader params] _]
                       (let [req (loader params)
                             req' (->> req
                                       (p/map #(put! commander [:resolve [loader params] %]))
                                       (p/error #(put! commander [:reject [loader params] %])))]
                         (assoc acc 
                                [loader (remove-dataloader-params params)] 
                                (specify! req'
                                  pt/IAbortable
                                  (abort! [this]
                                    (when (satisfies? pt/IAbortable req)
                                      (pt/abort! req)))))))
                     requests
                     pending)]
                (recur (merge inflight pending) {} requests'))
              :cancel      
              (let [{:keys [inflight pending requests]} (cancel-request inflight pending requests args)]
                (recur inflight pending requests))
              :resolve   
              (let [[req-ident res] args
                    [loader params] req-ident
                    req-promises    (get inflight req-ident)
                    time-now        (get-time-now)]
                (swap! cache-store$ assoc-in [:cache [loader (remove-dataloader-params params)]] 
                       {:data        res
                        :resolved-at time-now
                        :touched-at  time-now})
                (doseq [rp req-promises]
                  (p/resolve! rp res))
                (recur (dissoc inflight req-ident) 
                       pending
                       (dissoc requests req-ident)))
              :reject       
              (let [[req-ident err] args
                    req-promises    (get inflight req-ident)]
                (doseq [rp req-promises]
                  (p/reject! rp err))
                (recur (dissoc inflight req-ident)
                       pending
                       (dissoc requests req-ident)))
              (recur inflight pending requests)))))
      (go-loop []
        (let [[_ c] (alts! [requester-stopper (timeout (:request-interval config))])]
          (when (not= requester-stopper c)
            (put! commander [:kickoff])
            (recur))))
      (go-loop []
        (let [[_ c] (alts! [evicter-stopper (timeout (:evict-interval config))])]
          (when (not= evicter-stopper c)
            (swap! cache-store$ update :cache #(evict-lru % (:cache-size config)))
            (recur))))
      this))
  (abort [this loader params promise]
    (put! (:commander chans) [:cancel loader params promise]))

  (request [this loader]
    (request this loader nil))

  (request [this loader params]
    (let [loader' (if (keyword? loader) (get-in config [:dataloader/loaders loader]) loader)
          params-without-dataloader-params (remove-dataloader-params params)
          cached                           (get-in @cache-store$ [:cache [loader' params-without-dataloader-params]])
          is-cache-hit                     (cache-hit? cached params)
          is-stale                         (stale? cached params)
          cached-data                      (:data cached)]
      (cond
        (and is-cache-hit is-stale)
        (do
          (put! (:commander chans) [:request loader' params (p/deferred)])
          (p/promise cached-data))

        is-cache-hit
        (do
          (swap! cache-store$ assoc-in [:cache [loader' params-without-dataloader-params] :touched-at] (get-time-now))
          (p/promise cached-data))

        :else
        (let [deferred-result (p/deferred)]
          (put! (:commander chans) [:request loader' params deferred-result])
          deferred-result))))

  (evict [this loader])

  (evict [this loader params])

  (shutdown [this]
    (doseq [c (vals chans)]
      (close! c))
    (reset! cache-store$ nil)))

(defn constructor 
  ([] (constructor {}))
  ([config]
   (let [chans {:commander         (chan)
                :evicter-stopper   (chan)
                :requester-stopper (chan)}]
     (-> (->RequestManager (merge default-config config) chans (atom {}))
         start))))
