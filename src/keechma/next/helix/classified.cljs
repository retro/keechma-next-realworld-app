(ns keechma.next.helix.classified
  (:require [clojure.string :as str])
  (:require-macros [keechma.next.helix.classified :refer [defclassified]]))

(defn get-classes [props classes & togglers]
  (flatten (concat [classes] (mapv (fn [toggler] (toggler props)) togglers))))