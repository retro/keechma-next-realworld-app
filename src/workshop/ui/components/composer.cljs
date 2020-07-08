(ns workshop.ui.components.composer
  (:require [keechma.next.helix.core :refer [with-keechma use-sub dispatch]]
            [helix.hooks :as hooks]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]
            [keechma.next.controllers.router :as router]
            [app.ui.components.inputs :refer [wrapped-input]]))

(defnc ComposerRenderer [{:keys [list] :as props}]
  (let [[is-expanded set-is-expanded] (hooks/use-state false)]
    (d/div

      (d/div
        (d/button {:on-click #(set-is-expanded (not is-expanded))} "Compose email"))
      (when is-expanded
        (d/form
          {:on-submit (fn [e]
                        (.preventDefault e)
                        (dispatch props :compose-form :keechma.form/submit))}
          (wrapped-input {:keechma.form/controller :compose-form
                          :input/type :text
                          :input/attr :to
                          :placeholder "To"})
          (wrapped-input {:keechma.form/controller :compose-form
                          :input/type :textarea
                          :input/attr :body
                          :placeholder "Body" })
          (d/button
            {:class "btn btn-lg btn-primary pull-xs-right"} "Send In"))))))

(def Composer (with-keechma ComposerRenderer))