(ns app.ui.pages.home
  (:require [keechma.next.helix.core :refer [with-keechma]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <> suspense]]
            [helix.dom :as d]
            ["react" :as react]
            ["react-dom" :as rdom]
            [app.ui.components.articles :refer [Articles]]
            [keechma.next.controllers.pipelines :refer [throw-promise!]]))

(defnc TabsRenderer [])
(def Tabs (with-keechma TabsRenderer))

(defnc TagListRenderer
  [{:keechma/keys [use-sub use-meta-sub send!]}]
  (throw-promise! (use-meta-sub :tags) :keechma.on/start)
  (let [tags (use-sub :tags)]
    (<>
      (d/p "Popular Tags")
      (d/div
        {:class "tag-list"}
        (map
          (fn [{:keys [tag]}]
            (d/a
              {:class "tag-pill tag-default"
               :key tag
               :href (send! :router :get-url {:page "home" :subpage "tag" :detail tag})}
              tag))
          tags)))))
(def TagList (with-keechma TagListRenderer))

(defnc Home []
  (d/div
    {:class "home-page"}
    (d/div
      {:class "banner"}
      (d/div
        {:class "container"}
        (d/h1 {:class "logo-font"} "conduit")
        (d/p "A place to share your knowledge")))
    (d/div
      {:class "container page"}
      (d/div
        {:class "row"}
        (d/div
          {:class "col-md-9"}
          #_($ Tabs)
          ($ Articles))
        (d/div
          {:class "col-md-3"}
          (d/div
            {:class "sidebar"}
            (suspense
              {:fallback (d/div "Loading Tags...")}
              ($ TagList))))))))

