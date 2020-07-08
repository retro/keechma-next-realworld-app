(ns keechma.next.helix.classified
  (:require [helix.core :as helix :refer [<> $]]
            [clojure.string :as str]))

(defn get-type [component]
  (cond
    (string? component) component
    (keyword? component) (name component)
    :else component))

(defmacro defclassified [component-name component classes & togglers]
  (let [component-type (get-type component)]
    `(helix.core/defnc ~component-name ~'[props]
       {:helix/features {:fast-refresh false}}
       (let [class# (str/join " " (flatten [(get-classes ~'props ~classes ~@togglers) (:class ~'props) (:className ~'props)]))
             props# (dissoc ~'props :class :className)]
         (if (fn? ~component-type)
           (~component-type {:className class# :& props#} (:children ~'props))
           ($ ~component-type {:className class# & props#} (:children ~'props)))))))

(comment
  (clojure.pprint/pprint
    (macroexpand-1
      '(defclassified Heading :h1
         "text-bold"
         (fn [{:keys [props]}] "foo")))))