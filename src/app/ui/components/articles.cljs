(ns app.ui.components.articles
  (:require [keechma.next.helix.core :refer [with-keechma use-sub use-meta-sub send! call]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <> suspense]]
            [helix.dom :as d]
            ["react" :as react]
            ["react-dom" :as rdom]
            [app.util :refer [format-date]]
            [keechma.next.controllers.pipelines :refer [throw-promise!]]
            [app.settings :as settings]
            [app.ui.components.favorite-button :refer [FavoriteButton]]
            [keechma.next.controllers.hashchange-router :refer [get-url]]))

(defnc Article
  [{:keys [article]
    :keechma/keys [app]}]
  (let [{:keys [author tagList]} article
        author-url (call app :router get-url {:page "profile" :subpage (:username author)})]
    (d/div
      {:class "article-preview"}
      (d/div
        {:class "article-meta"}
        (d/a
          {:href author-url}
          (d/img {:src (:image author)}))
        (d/div
          {:class "info"}
          (d/a
            {:href author-url}
            (:username author))
          (d/span
            {:class "date"}
            (format-date (:createdAt article))))
        ($ FavoriteButton {:article article :size :small}))
      (d/a
        {:class "preview-link"
         :href (send! app :router :get-url {:page "article" :subpage (:slug article)})}
        (d/h1 (:title article))
        (d/p (:description article))
        (d/span "Read more...")
        (when (seq tagList)
          (d/ul
            {:class "tag-list"}
            (map
              (fn [{:keys [tag]}]
                (d/li
                  {:class "tag-default tag-pill tag-outline"
                   :key tag}
                  tag))
              tagList)))))))

(defnc Loading []
  (d/div {:class "article-preview"} "Loading Articles..."))

(defnc InnerArticlesRenderer
  [{:keechma/keys [app] :as props}]
  (let [articles (use-sub app :articles)]
    (throw-promise! (use-meta-sub app :articles) :keechma.on/start)
    (<>
      (map (fn [a] ($ Article {:key (:slug a) :article a & props})) articles))))

(defnc Pagination
  [{:keechma/keys [app]}]
  (let [articles-meta (use-meta-sub app :articles)
        route (use-sub app :router)
        page (js/parseInt (or (:p route) "1") 10)
        article-count (get-in articles-meta [:response :count] 0)
        page-count (* 1 (.ceil js/Math (/ article-count settings/articles-per-page)))]
    (when (< 1 page-count)
      (d/nav
        (d/ul
          {:class "pagination"}
          (let [_ (js/console.time)
                res (doall (map
                             (fn [p]
                               (d/li
                                 {:key p
                                  :class ["page-item" (when (= p page) "active")]}
                                 (d/a {:class "page-link" :href (call app :router get-url (assoc route :p p))} p)))
                             (range 1 (inc page-count))))]
            (js/console.timeEnd)
            res))))))

(defnc ArticlesRenderer
  [props]
  (d/div
    #_(suspense
      {:fallback ($ Loading)
       :maxDuration 1000}
      ($ InnerArticlesRenderer {& props}))
    ($ Pagination {& props})))

(def Articles (with-keechma ArticlesRenderer))