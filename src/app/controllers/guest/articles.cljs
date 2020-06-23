(ns app.controllers.guest.articles
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.controllers.entitydb :as edb]
            [keechma.next.controllers.dataloader :as dl]
            [keechma.next.toolbox.pipeline :as pp :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [app.settings :as settings]))

(derive :guest/articles ::pipelines/controller)

(defn add-articles-pagination-param [params {:keys [p]}]
  (if p
    (let [offset (* (dec (js/parseInt p 10)) settings/articles-per-page)]
      (assoc params :offset offset))
    params))

(defn debug [v]
  (println v)
  v)

(def -load-articles!
  (-> (pipeline! [value {:keys [meta-state*] :as ctrl}]
        (dl/req ctrl :dataloader api/get-public-articles value {:keechma.dataloader/max-stale 100 :keechma.dataloader/stale-while-revalidate true})
        (edb/insert-collection! ctrl :entitydb :article :article/list (:data value))
        (pswap! meta-state* assoc :response (:meta value)))
      (pp/set-queue :load-articles!)
      pp/use-existing
      pp/restartable))

(def load-articles!
  (pipeline! [value {:keys [meta-state* deps-state*] :as ctrl}]
    (add-articles-pagination-param {} (:router @deps-state*))
    (when (not= value (:params @meta-state*))
      (pipeline! [value {:keys [meta-state*]}]
        (pswap! meta-state* assoc :params value)
        -load-articles!))))

(defn get-req-params [value]
  {})

(def pipelines
  {:keechma.on/start load-articles!
   :keechma.on/deps-change load-articles!
   :keechma.on/stop (pipeline! [_ ctrl]
                      (edb/remove-collection! ctrl :entitydb :article/list))})

(defmethod ctrl/prep :guest/articles [ctrl]
  (pipelines/register ctrl pipelines))

(defmethod ctrl/derive-state :guest/articles [_ state {:keys [entitydb]}]
  (edb/get-collection entitydb :article/list [(edb/include :author)]))