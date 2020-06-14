(ns keechma.next.toolbox.protocols-test
  (:require [cljs.test :refer-macros [deftest testing is async use-fixtures]]
            [keechma.next.toolbox.protocols :as pt]
            [promesa.core :as p]))

(deftest abortable-promise
  (let [abortable (specify! (p/deferred) 
                    pt/IAbortable
                    (abort! [this]
                      (p/resolve! this :aborted)))]
    (async done
           (->> abortable
                (p/map (fn [res]
                         (is (= :aborted res))
                         (done))))
           (pt/abort! abortable))))
