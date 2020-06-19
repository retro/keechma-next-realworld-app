(ns app.controllers.user.articles
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [app.settings :as settings]))

(derive :user/articles ::pipelines/controller)

(defn add-articles-pagination-param [params {:keys [p]}]
  (if p
    (let [offset (* (dec (js/parseInt p 10)) settings/articles-per-page)]
      (assoc params :offset offset))
    params))

(def pipelines
  {:keechma.on/start (pipeline! [value {:keys [state* meta-state* deps-state*]}]
                       (api/get-user-articles (:jwt @deps-state*) (add-articles-pagination-param {} (:router @deps-state*)))
                       (preset! state* (:data value))
                       (pswap! meta-state* assoc :response (:meta value)))})

(defmethod ctrl/prep :user/articles [ctrl]
  (pipelines/register ctrl pipelines))
