(ns keechma.next.toolbox.dataloader.request-manager-test
  (:require [cljs.test :refer-macros [deftest testing is async use-fixtures]]
            [keechma.next.toolbox.dataloader :as dl]
            [promesa.core :as p] 
            [keechma.next.toolbox.dataloader.request-manager :as rm]))

(deftest basic
  (let [rm (rm/constructor)]))
