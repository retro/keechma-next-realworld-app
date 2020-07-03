(ns app.controllers.user.editor-form
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [keechma.next.controllers.form :as form]
            [app.validators :as v]
            [promesa.core :as p]
            [clojure.string :as str]
            [keechma.next.controllers.entitydb :as edb]
            [keechma.next.controllers.dataloader :as dl]))

(derive :user/editor-form ::pipelines/controller)

(defn process-tag-list-out [value]
  (let [tag-list (map str/trim (str/split (:tags value) #","))]
    (-> value
        (dissoc :tags)
        (assoc :tagList tag-list))))

(defn process-tag-list-in [value]
  (let [tags (str/join ", " (map :tag (:tagList value)))]
    (assoc value :tags tags)))

(def pipelines
  {:keechma.form/get-data    (pipeline! [value {:keys [deps-state*] :as ctrl}]
                                        (let [{:keys [jwt article router]} @deps-state*]
                                          (if (seq article)
                                            (process-tag-list-in article)
                                            (pipeline! [value ctrl]
                                                       (dl/req ctrl :dataloader api/article-get {:article-slug (:slug router) :jwt jwt})
                                                       (process-tag-list-in value)))))

   :keechma.form/submit-data (pipeline! [value {:keys [deps-state*] :as ctrl}]
                                        (let [jwt (:jwt @deps-state*)
                                              slug (:slug value)
                                              new? (not (boolean slug))]
                                          (if new?
                                            (api/article-create
                                              {:jwt     jwt
                                               :article (process-tag-list-out value)})
                                            (api/article-update
                                              {:jwt          jwt
                                               :article-slug slug
                                               :article      (process-tag-list-out value)})))
                                        (pipeline! [value ctrl]
                                                   (edb/insert-named! ctrl :entitydb :article :article/current value)
                                                   (ctrl/dispatch ctrl :router :redirect! {:page "article" :subpage (:slug value)})))})

(defmethod ctrl/prep :user/editor-form [ctrl]
  (pipelines/register ctrl (form/wrap pipelines (v/to-validator {:title       [:not-empty]
                                                                 :description [:not-empty]
                                                                 :body        [:not-empty]}))))
