(ns app.ui.components.favorite-button
  (:require [keechma.next.helix.core :refer [with-keechma]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <> suspense]]
            [helix.dom :as d]
            ["react" :as react]
            ["react-dom" :as rdom]
            [app.util :refer [format-date]]
            [keechma.next.controllers.pipelines :refer [throw-promise!]]
            [app.settings :as settings]))

(defnc FavoriteButtonRenderer
  [{:keechma/keys [send!]
    :keys [article size]}]
  (let [{:keys [favorited favoritesCount]} article
        is-small (= :small size)]
    (d/button
      {:class ["btn btn-sm"
               (if favorited "btn-primary " "btn-outline-primary")
               (when is-small "pull-xs-right")]
       :onClick #(send! :user-actions :toggle-favorite article)}
      (d/i {:class "ion-heart"})
      " "
      (if is-small
        favoritesCount
        (str (if favorited "Unfavorite" "Favorite") " Post (" favoritesCount ")")))))

(def FavoriteButton (with-keechma FavoriteButtonRenderer))