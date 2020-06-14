(ns keechma.next.toolbox.pipeline
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.walk :refer [prewalk]]))

(defn extract-pipeline-parts [args body]
  (let [last-block (last body)
        has-rescue-block? (and (seq? last-block) (= "rescue!" (name (first last-block))))
        [begin-body rescue] (if has-rescue-block? [(drop-last body) (last body)] [body nil])
        [_ rescue-args & rescue-body] rescue]
    {:begin-args args
     :begin-body begin-body
     :rescue-args rescue-args
     :rescue-body rescue-body}))

(defn expand-body [args body]
  (into [] (map (fn [f] `(fn ~args {:val ~f :repr ~(str/trim (with-out-str (pprint f)))})) body)))

(defn begin-forms [acc {:keys [begin-args begin-body]}]
  (if (not= 2 (count begin-args))
    (throw (ex-info "Pipeline accepts two arguments: value and context" {}))
    (assoc acc :begin (expand-body begin-args begin-body))))

(defn rescue-forms [acc {:keys [begin-args rescue-args rescue-body]}]
  (if (or (nil? rescue-args) (nil? rescue-body))
    acc
    (if (not= 1 (count rescue-args))
      (throw (ex-info "Pipeline catch block accepts one argument: error" {}))
      (assoc acc :rescue (expand-body (into [] (concat begin-args rescue-args)) rescue-body)))))

(defn make-pipeline [id args])

(defn prepare-pipeline [args body]
  (let [pipeline-parts (extract-pipeline-parts args body)
        id (keyword (gensym ::pipeline))]
    `(keechma.next.toolbox.pipeline/make-pipeline
      ~id
      ~(-> {}
           (begin-forms pipeline-parts)
           (rescue-forms pipeline-parts)))))

(defmacro pipeline! [args & body]
  (prepare-pipeline args (or body `())))
