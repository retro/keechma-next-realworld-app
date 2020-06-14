(ns keechma.next.spec
  (:require
    [cljs.spec.alpha :as s]))

(defn dynamic-config? [{:keys [:keechma.controller/params]}]
  (= :dynamic (first params)))

(defn static-config? [{:keys [:keechma.controller/params]}]
  (= :static (first params)))

(defn dep? [val]
  (or (keyword? val)
      (and (vector? val) (= 1 (count val)))
      (and (vector? val) (= 2 (count val)))))

(defn ensure-controller-type [[k v]]
  [k (update v :keechma.controller/type #(or % (if (vector? k) (first k) k)))])

(defn process-controller-params [[params-variant controller]]
  (-> controller
      (assoc :keechma.controller.params/variant params-variant)
      (update :keechma.controller/params last)))

(defn inline-controller-variant [[controller-variant [controller-type controller]]]
  [controller-type (assoc controller :keechma.controller/variant controller-variant)])

(defn inline-factory-produced-params-variant [v]
  (let [[variant params] (:keechma.controller/params v)]
    (assoc v :keechma.controller/params params
             :keechma.controller.params/variant variant)))

(defn inline-app-variant [[variant app]]
  (assoc app :keechma.app/variant variant))

(s/def :keechma.controller.factory/produce
  fn?)

(s/def :keechma.controller/deps
  (s/coll-of dep? :kind vector? :min-count 1))

(s/def :keechma.controller.name/singleton
  keyword?)

(s/def :keechma.controller.name/identity
  (s/tuple keyword? (complement nil?)))

(s/def :keechma.controller.name/factory
  (s/tuple keyword?))

(s/def :keechma.controller/type
  (s/and keyword? #(isa? % :keechma/controller)))

(s/def :keechma.controller.params/dynamic
  fn?)

(s/def :keechma.controller.params/static
  (s/and (complement fn?) boolean))

(s/def :keechma.controller/params
  (s/or :dynamic :keechma.controller.params/dynamic
        :static :keechma.controller.params/static))

(s/def :keechma.controller.config/dynamic
  (s/and (s/keys :req [:keechma.controller/deps
                       :keechma.controller/params
                       :keechma.controller/type])
         dynamic-config?))

(s/def :keechma.controller.config/static
  (s/and (s/keys :req [:keechma.controller/params
                       :keechma.controller/type])
         static-config?))

(s/def :keechma.controller.config/factory
  (s/keys :req [:keechma.controller/deps
                :keechma.controller/type
                :keechma.controller.factory/produce]))

(s/def :keechma.controller/config
  (s/and
    (s/or :static :keechma.controller.config/static
          :dynamic :keechma.controller.config/dynamic)
    (s/conformer process-controller-params)))

(s/def :keechma.controller.variant/singleton
  (s/tuple :keechma.controller.name/singleton :keechma.controller/config))

(s/def :keechma.controller.variant/identity
  (s/tuple :keechma.controller.name/identity :keechma.controller/config))

(s/def :keechma.controller.variant/factory
  (s/tuple :keechma.controller.name/factory :keechma.controller.config/factory))

(s/def :keechma/controller
  (s/and
    (s/conformer ensure-controller-type)
    (s/or
      :singleton :keechma.controller.variant/singleton
      :identity :keechma.controller.variant/identity
      :factory :keechma.controller.variant/factory)
    (s/conformer inline-controller-variant)))

(s/def :keechma.controller.factory/produced
  (s/and
    (s/keys :req [:keechma.controller/params]
            :opt [:keechma.controller/deps])
    (s/conformer inline-factory-produced-params-variant)))

(s/def :keechma/controllers
  (s/and (s/map-of #(or (keyword? %) (vector? %)) map?)
         (s/coll-of :keechma/controller :into {})))

(s/def :keechma/app
  (s/keys
    :req [:keechma/controllers]
    :opt [:keechma/apps]))

(s/def :keechma/nested-app
  (s/and
    (s/or
      :dynamic (s/keys :req [:keechma.app/load :keechma.app/should-run? :keechma.app/deps])
      :static (s/merge :keechma/app (s/keys :req [:keechma.app/should-run? :keechma.app/deps])))
    (s/conformer inline-app-variant)))

(s/def :keechma.app/should-run? fn?)
(s/def :keechma.app/load fn?)
(s/def :keechma.app/deps (s/merge :keechma.controller/deps))

(s/def :keechma/apps
  (s/map-of keyword? :keechma/nested-app))

(s/def :keechma/app-instance map?)