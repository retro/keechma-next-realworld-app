(ns app.controllers.guest.articles
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.controllers.entitydb :as edb]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [app.settings :as settings]))

(derive :guest/articles ::pipelines/controller)

(defn add-articles-pagination-param [params {:keys [p]}]
  (if p
    (let [offset (* (dec (js/parseInt p 10)) settings/articles-per-page)]
      (assoc params :offset offset))
    params))

(def pipelines
  {:keechma.on/start (pipeline! [value {:keys [meta-state* deps-state*] :as ctrl}]
                       (api/get-public-articles (add-articles-pagination-param {} (:router @deps-state*)))
                       (ctrl/call ctrl :entitydb edb/insert-collection! :article :article/list (:data value))
                       (pswap! meta-state* assoc :response (:meta value)))
   :keechma.on/stop (pipeline! [_ ctrl]
                      (ctrl/call ctrl :entitydb edb/remove-collection! :article/list))})

(defmethod ctrl/prep :guest/articles [ctrl]
  (pipelines/register ctrl pipelines))

(defmethod ctrl/derive-state :guest/articles [_ state {:keys [entitydb]}]
  (edb/get-collection entitydb :article/list [(edb/rel-include :author)]))