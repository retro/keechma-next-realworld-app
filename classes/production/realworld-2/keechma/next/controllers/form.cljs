(ns keechma.next.controllers.form
  (:require [keechma.next.toolbox.pipeline :as pp :refer-macros [pipeline!] :refer [pswap! preset!]]
            [clojure.set :as set]
            [forms.util :refer [key-to-path]]
            [forms.dirty :refer [calculate-dirty-fields]]
            [forms.core]))

(def form-pipeline-api-keys #{:keechma.form/submit-data
                              :keechma.form/get-data
                              :keechma.on/start})

(defn make-initial-state [value]
  {:submit-attempted? false
   :dirty-paths #{}
   :cached-dirty-paths #{}
   :data value
   :initial-data value
   :state {}})

(defn get-errors [meta-state]
  (get-in meta-state [::form :errors]))

(defn get-data [meta-state]
  (get-in meta-state [::form :data]))

(defn get-initial-data [meta-state]
  (get-in meta-state [::form :initial-data]))


(defn get-data-in [meta-state path]
  (get-in (get-data meta-state) (key-to-path path)))=

(defn get-errors-in [meta-state path]
  (let [form-state (::form meta-state)
        path' (key-to-path path)
        is-dirty (or (contains? (:cached-dirty-paths form-state) path')
                     (contains? (:dirty-paths form-state) path'))]
    (when is-dirty
      (get-in (:errors form-state) path'))))

(defn path-valid? [meta-state path]
  (nil? (get-errors-in meta-state path)))

(def path-invalid? (complement path-valid?))

(defn valid? [meta-state]
  (not (seq (get-in meta-state [::form :errors]))))

(def invalid? (complement valid?))

(defn submit-attempted? [meta-state]
  (get-in meta-state [::form :submit-attempted?]))

(defn mark-dirty-and-validate
  ([meta-state validator] (mark-dirty-and-validate meta-state validator true))
  ([meta-state validator only-dirty]
   (let [data (get-data meta-state)
         initial-data (get-initial-data meta-state)]
     (if only-dirty
       (let [errors (when validator (validator data))
             dirty-paths (calculate-dirty-fields initial-data data)]
         (update meta-state ::form merge {:errors errors :dirty-paths (set dirty-paths)}))
       (let [errors (when validator (validator data))
             dirty-paths (set (forms.core/errors-keypaths errors))
             cached-dirty-paths (set (get-in meta-state [::form :cached-dirty-paths]))]

         (update meta-state ::form merge {:errors errors
                                          :dirty-paths dirty-paths
                                          :cached-dirty-paths (set/union dirty-paths cached-dirty-paths)}))))))



(defn assoc-data-in [meta-state path value]
  (let [path' (key-to-path path)]
    (assoc-in meta-state (concat [::form :data] path') value)))

(defn handle-on-change [meta-state validator payload]
  (let [path (key-to-path (:attr payload))
        should-validate-immediately? (or (:validate-immediately? payload) (path-invalid? meta-state path))]
    (cond-> meta-state
      true (assoc-data-in (:attr payload) (:value payload))
      should-validate-immediately? (mark-dirty-and-validate validator))))

(defn handle-on-blur [meta-state validator payload]
  (mark-dirty-and-validate meta-state validator))

(defn handle-on-submit [meta-state validator payload]
  (-> meta-state
      (mark-dirty-and-validate validator false)
      (assoc-in [::form :submit-attempted?] true)))


(defn make-form-pipelines [form-pipeline-api validator]
  (let [submit-data (:keechma.form/submit-data form-pipeline-api)]
    {:keechma.form/submit (-> (pipeline! [value {:keys [meta-state*]}]
                                (pswap! meta-state* handle-on-submit validator value)
                                (when (and submit-data (valid? @meta-state*))
                                  (pipeline! [_ {:keys [meta-state*]}]
                                    (get-data @meta-state*)
                                    submit-data)))
                              pp/use-existing
                              pp/dropping)
     :keechma.form/validate (pipeline! [value ctrl])
     :keechma.form.on/change (pipeline! [value {:keys [meta-state*]}]
                               (pswap! meta-state* handle-on-change validator value))
     :keechma.form.on/blur (pipeline! [value {:keys [meta-state*]}]
                             (pswap! meta-state* handle-on-blur validator value))
     :keechma.on/start (pipeline! [value {:keys [meta-state*]}]
                         (let [value' value]
                           (pipeline! [_ _]
                             (:keechma.on/start form-pipeline-api)
                             value'))
                         (or (:keechma.form/get-data form-pipeline-api) {})
                         (pswap! meta-state* assoc ::form (make-initial-state value)))}))

(defn wrap [pipelines validator]
  (let [form-pipeline-api (select-keys pipelines form-pipeline-api-keys)]
    (-> (apply dissoc pipelines form-pipeline-api-keys)
        (merge (make-form-pipelines form-pipeline-api validator)))))