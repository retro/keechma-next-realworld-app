(ns keechma.next.toolbox.dataloader
  (:refer-clojure :exclude [resolve])
  (:require [promesa.core :as p]
            [cljs.core.async :refer [<! chan put! close! alts!]]
            [keechma.next.toolbox.dataloader.request-manager :as rm]
            [keechma.next.toolbox.protocols :as pt]
            [keechma.next.toolbox.exceptions :refer [aborted-ex-info]]
            [keechma.next.toolbox.util :refer [error? as-error promise->chan]])
  (:require-macros [cljs.core.async.macros :as m :refer [go-loop]]))

(def debug? ^boolean goog.DEBUG)

(defn pp [& args]
  (when debug?
    (apply js/console.log (map (fn [a] (with-out-str (cljs.pprint/pprint a))) args))))

(defrecord Request [loader params])

(defn request? [request]
  (= Request (type request)))

(defn request
  ([loader] (request loader nil))
  ([loader params]
   (->Request loader params)))

(defn make-placeholder-vector [current size]
  (or current (vec (repeat size nil))))

(declare walk-leaf)
(declare walk-leafs)

(defn walk-leafs [leafs inflight cursor path]
  (reduce
   (fn [acc leaf]
     (walk-leaf leaf inflight acc path))
   cursor
   leafs))

(defn walk-leaf
  ([leaf inflight cursor] (walk-leaf leaf inflight cursor []))
  ([leaf inflight cursor path]
   ;; Figure out how can we optimize this. Maybe we could cache
   ;; leaf hash, and not walk through it if it didn't change
   ;; since the last iteration
   (if leaf
     (if (request? leaf)
       (if (nil? (get inflight path))
         (assoc-in cursor [:requests path] leaf)
         cursor)
       (let [[tag & children] leaf]
         (if (= :<> tag)
           (walk-leafs children inflight cursor path)
           (let [[request & children'] children
                 path' (conj path tag)
                 is-request (request? request)]

             (cond
               (and is-request (nil? (get inflight path')))
               (assoc-in cursor [:requests path'] request)

               (or (not is-request)
                   (= :done (get inflight path')))
               (let [leafs (if is-request children' children)]
                 (cond
                   (and (= 1 (count leafs)) (seq? (first leafs)))
                   (reduce-kv
                    (fn [acc idx leaf]
                      (walk-leaf leaf inflight acc (conj path' idx)))
                    (update-in cursor (concat [:resolved] path') #(make-placeholder-vector % (count (first leafs))))
                    (vec (first leafs)))

                   (and (< 1 (count leafs)) (some seq? leafs))
                   (throw (ex-info "List must be only leaf." {}))

                   :else (walk-leafs (vec leafs) inflight cursor path')))

               :else cursor)))))
     cursor)))

(defn start-requests [requests context]
  (let [request-manager (:dataloader/request-manager context)]
    (->> (map
          (fn [[path {:keys [loader params]}]]
            (let [req (if request-manager (rm/request request-manager loader params) (loader params))
                  formatted-req (p/then req (fn [res] [path res]))]
              [path (specify! formatted-req
                      pt/IAbortable
                      (abort! [this]
                        (if request-manager
                          (rm/abort request-manager loader params req)
                          (when (satisfies? pt/IAbortable req)
                            (pt/abort! req)))))]))
          requests)
         (into {}))))

(defn process [datasources params resolved]
  (if (fn? datasources) (datasources params resolved) datasources))

(defn resolve [datasources params resolved context]
  (let [{:keys [resolved requests]} (walk-leaf (process datasources params resolved) {:resolved resolved} {})
        inflight (start-requests requests context)
        deferred-result (p/deferred)
        aborter-chan (chan)]
    (go-loop [resolved resolved
              inflight inflight
              requests requests]
      (if (empty? requests)
        (p/resolve! deferred-result resolved)
        (let [res-chan (->> (vals inflight)
                            (filter p/promise?)
                            p/all
                            promise->chan)
              [res c] (alts! [res-chan aborter-chan])]
          (if (= c aborter-chan)
            (let [inflight-promises (filter p/promise? (vals inflight))]
              (doseq [p inflight-promises]
                (when (satisfies? pt/IAbortable p)
                  (pt/abort! p)))
              (p/reject! deferred-result (aborted-ex-info)))
            (if (error? res)
              (p/reject! deferred-result res)
              (let [resolved' (reduce (fn [acc [path value]] (assoc-in acc path value)) resolved res)
                    inflight' (reduce (fn [acc [path _]] (assoc acc path :done)) inflight res)
                    cursor (walk-leaf (process datasources params resolved') inflight' {:resolved resolved'})
                    requests' (:requests cursor)]
                (recur (:resolved cursor)
                       (merge inflight' (start-requests requests' context))
                       (-> (reduce (fn [acc [path _]] (dissoc acc path)) requests res)
                           (merge requests')))))))))
    (specify! deferred-result
      pt/IAbortable
      (abort! [this]
        (close! aborter-chan)))))
