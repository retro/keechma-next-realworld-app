(ns workshop.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]))

(defnc MainRenderer
  [props]
  (d/h1 "Hello VBT"))

(def Main (with-keechma MainRenderer))