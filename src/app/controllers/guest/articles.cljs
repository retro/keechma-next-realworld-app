(ns app.controllers.guest.articles
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.controllers.entitydb :as edb]
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

(def load-articles!
  (-> (pipeline! [value {:keys [meta-state* deps-state*] :as ctrl}]
        (pswap! meta-state* assoc :params value)
        (api/get-public-articles (add-articles-pagination-param {} (:router @deps-state*)))
        (edb/insert-collection! ctrl :entitydb :article :article/list (:data value))
        (pswap! meta-state* assoc :response (:meta value))
        (rescue! [error]
                 (js/console.error error)))
      (pp/set-queue :load-articles!)
      pp/use-existing
      pp/restartable))

(defn get-req-params [value]
  {})

(def pipelines
  {:keechma.on/start (pipeline! [value _]
                       (get-req-params value)
                       load-articles!)
   :keechma.on/deps-change (pipeline! [value {:keys [meta-state*]}]
                             (let [new-params (get-req-params value)]
                               (when-not (= new-params (:params @meta-state*))
                                 load-articles!)))
   :keechma.on/stop (pipeline! [_ ctrl]
                      (ctrl/call ctrl :entitydb edb/remove-collection! :article/list))})

(defmethod ctrl/prep :guest/articles [ctrl]
  (pipelines/register ctrl pipelines))

(defmethod ctrl/derive-state :guest/articles [_ state {:keys [entitydb]}]
  (edb/get-collection entitydb :article/list [(edb/include :author)]))