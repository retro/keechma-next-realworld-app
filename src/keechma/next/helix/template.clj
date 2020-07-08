(ns keechma.next.helix.template
  (:require [helix.core :as helix :refer [<> $]]
            [clojure.walk :refer [postwalk prewalk]]
            [clojure.set :as set]))

(defn slot [slot-name & body])
(defn optional-slot [slot-name & body])
(defn configurable [configurable-name])

(defn internal-fn? [fn-name v]
  (and
    (list? v)
    (symbol? (first v))
    (= fn-name (name (first v)))))


(defn add-slot-as-descendant [cursor slot-name]
  (let [path (:path cursor)]
    (reduce
      (fn [acc ancestor-name]
        (update-in acc [:slots ancestor-name :descendants] conj slot-name))
      cursor
      path)))

(defn cursor-with-slot [cursor [_ slot-name & _] is-optional]
  (let [parent  (last (:path cursor))
        cursor' (if parent (update-in cursor [:slots parent :children] conj slot-name) cursor)]
    (-> cursor'
      (update :path conj slot-name)
      (add-slot-as-descendant slot-name)
      (assoc-in [:slots slot-name] {:children    #{}
                                    :descendants #{}
                                    :configurables #{}
                                    :parent      parent
                                    :is-optional is-optional}))))

(defn cursor-with-configurable [cursor [_ configurable-name & _]]
  (let [parent (last (:path cursor))
        cursor' (if parent (update-in cursor [:slots parent :configurables] conj configurable-name) cursor)]
    (assoc-in cursor' [:configurables configurable-name] {:parent parent})))

(defn extract-internal-fns
  ([body] (extract-internal-fns body {:path [] :configurables {} :slots {}}))
  ([body cursor]
   (reduce
     (fn [acc v]
       (let [is-slot          (internal-fn? "slot" v)
             is-optional-slot (internal-fn? "optional-slot" v)]
         (cond
           (or is-slot is-optional-slot)
           (update (extract-internal-fns v (cursor-with-slot acc v is-optional-slot)) :path #(vec (drop-last %)))

           (internal-fn? "configurable" v)
           (extract-internal-fns v (cursor-with-configurable acc v))

           (coll? v)
           (extract-internal-fns v acc)
           :else acc)))
     cursor
     body)))

;; TODO: optional-slot should check if any of descendants is filled out
;; TODO: slots should wrap their body into anonymous function and pass it as argument if fill is called with fn

(defmacro defnt [type params & body]
  (let [opts?        (map? (first body))            ;; whether an opts map was passed in
        opts         (if opts?
                       (first body)
                       {})
        body         (if opts?
                       (rest body)
                       body)
        ;; feature flags to enable by default
        default-opts {:helix/features {:fast-refresh false}}
        props-sym (gensym 'props-)
        ref-sym (gensym 'ref-)
        params-sym   (if (= 1 (count params)) `[~props-sym] `[~props-sym ~ref-sym])
        internal-fns (extract-internal-fns body)]
    `(helix.core/defnc ~type ~params-sym
       ;; we use `merge` here to allow indidivual consumers to override feature
       ;; flags in special cases
       ~(merge default-opts opts)
       (let [~params ~params-sym]
         ~@(prewalk
             (fn [node]
               (let [is-slot          (internal-fn? "slot" node)
                     is-optional-slot (internal-fn? "optional-slot" node)]
                 (cond
                   is-slot
                   (let [[_ slot-name & slot-body] node
                         wrapped-slot `(fn [] ~(if (= 1 (count slot-body)) `~(first slot-body) `(<> ~@slot-body)))]
                     `(let [wrapped-slot# ~wrapped-slot
                            filled-slot#  (get-in ~props-sym [::slots ~slot-name])]
                        (if (fn? filled-slot#)
                          (filled-slot# wrapped-slot#)
                          (or filled-slot# (wrapped-slot#)))))

                   is-optional-slot
                   (let [[_ slot-name & slot-body] node
                         wrapped-slot `(fn [] ~(if (= 1 (count slot-body)) `~(first slot-body) `(<> ~@slot-body)))]
                     `(let [wrapped-slot# ~wrapped-slot
                            filled-slot#  (get-in ~props-sym [::slots ~slot-name])]
                        (when (some (::slots ~props-sym) ~(conj (get-in internal-fns [:slots slot-name :descendants]) slot-name))
                          ~internal-fns
                          (if (fn? filled-slot#)
                            (filled-slot# wrapped-slot#)
                            (or filled-slot# (wrapped-slot#))))))

                   (internal-fn? "configurable" node)
                   (let [[_ configurable-name default-props] node]
                     `{& (let [default-props#       ~default-props
                               filled-configurable# (get-in ~props-sym [::configurables ~configurable-name])]
                           (if (fn? filled-configurable#)
                             (filled-configurable# default-props#)
                             (or filled-configurable# default-props#)))})
                   :else node)))
             body)))))


(comment
  (defnt Bar [props]
    (d/div
      (slot :bar-main)))

  (clojure.pprint/pprint
    (macroexpand-1
      '(defnt Templated [props]
         (d/div
           (slot :foo "bar")))))

  (clojure.pprint/pprint
    (macroexpand-1
      '(defnt Foo [props]
         (d/div
           #_(slot :main
             ($ Bar (configurable :main/article {:foo :bar}))
             (slot :main/foo
               (d/div
                 (slot :main.foo/bar
                   (d/div "Hello")))))
           (optional-slot :comments
             (d/div
               (slot :comments/body))))))

    #_($ Article
      (-> {:foo "bar"}
        (configure :main/article {:bla :bla})
        (fill-slot :main ($ ArticleBody))
        (fill-slot :comments/body ($ Comments))
        (fill-slot :main/foo (fn [super]
                               ($ Bla (super)))))

      ($ Child))))