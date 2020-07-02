(ns app.controllers.user.user-actions
  (:require [keechma.next.controller :as ctrl]
            [keechma.next.controllers.pipelines :as pipelines]
            [keechma.next.toolbox.pipeline :as pp :refer [pswap! preset!] :refer-macros [pipeline!]]
            [app.api :as api]
            [promesa.core :as p]
            [app.settings :as settings]
            [keechma.next.controllers.entitydb :as edb]))

(derive :user/user-actions ::pipelines/controller)

(defn optimistic-toggle-favorite [value]
  (if (:favorited value)
    (-> value
        (assoc :favorited false)
        (update :favoritesCount dec))
    (-> value
        (assoc :favorited true)
        (update :favoritesCount inc))))

(def pipelines
  {:logout (pipeline! [value ctrl]
             (ctrl/dispatch ctrl :jwt :clear)
             (ctrl/dispatch ctrl :router :redirect! {:page "home"}))
   :toggle-favorite (-> (pipeline! [value {:keys [deps-state*] :as ctrl}]
                          (let [api-fn (if (:favorited value) api/favorite-delete api/favorite-create)]
                            (pipeline! [value {:keys [deps-state*] :as ctrl}]
                              (edb/insert-entity! ctrl :entitydb :article (optimistic-toggle-favorite value))
                              ;; (p/delay 2000)
                              (api-fn {:article-slug (:slug value) :jwt (:jwt @deps-state*)})
                              (edb/insert-entity! ctrl :entitydb :article value))))
                        (pp/set-queue #(:slug %))
                        pp/enqueued)})

(defmethod ctrl/prep :user/user-actions [ctrl]
  (pipelines/register ctrl pipelines))
