(ns workshop.ui.components.emails-list
  (:require [keechma.next.helix.core :refer [with-keechma use-sub]]
            [keechma.next.helix.lib :refer [defnc]]
            [helix.core :as hx :refer [$ <>]]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]
            [keechma.next.controllers.router :as router]))

(defnc Email [{:keys [email index] :as props}]
  (let [route (use-sub props :router)]
    (d/li
      {:class (when-not (zero? index) "border-t border-gray-200")}
      (d/a
        {:class "block hover:bg-gray-50 focus:outline-none focus:bg-gray-50 transition duration-150 ease-in-out"
         :href (router/get-url props :router (assoc route :id (:id email)))}
        (d/div
          {:class "flex items-center px-4 py-4 sm:px-6"}
          (d/div
            {:class "min-w-0 flex-1 flex items-center"}
            (d/div
              {:class "flex-shrink-0"}
              (d/img
                {:class "h-12 w-12 rounded-full"
                 :src (str (:avatar email) "?w=80&" (:from email))}))
            (d/div
              {:class "min-w-0 flex-1 px-4 md:grid md:grid-cols-2 md:gap-4"}
              (d/div
                (d/div
                  {:class "text-sm leading-5 font-medium text-indigo-600 truncate"}
                  (:subject email))
                (d/div
                  {:class "mt-2 flex items-center text-sm leading-5 text-gray-500"}
                  (d/svg
                    {:class "flex-shrink-0 mr-1.5 h-5 w-5 text-gray-400"
                     :viewBox "0 0 20 20"
                     :fill "currentColor"}
                    (d/path {:d "M2.003 5.884L10 9.882l7.997-3.998A2 2 0 0016 4H4a2 2 0 00-1.997 1.884z"})
                    (d/path {:d "M18 8.118l-8 4-8-4V14a2 2 0 002 2h12a2 2 0 002-2V8.118z"}))
                  (d/span
                    {:class "truncate"}
                    (:from email))))
              (d/div
                {:class "hidden md:block"}
                (d/div
                  (d/div
                    {:class "text-right text-sm leading-5 text-gray-900"}
                    (when (:isImportant email)
                      (d/span
                        {:class "inline-flex items-center px-2 py-0.5 rounded text-xs font-medium leading-4 bg-yellow-100 text-yellow-800"}
                        "Important")))))))))))
  )

(defnc EmailsListRenderer [{:keys [list] :as props}]
  (let [emails (use-sub props :emails-list)]
    (if (seq emails)
      (d/div
        {:class "bg-white shadow overflow-hidden sm:rounded-md"}
        (d/ul
          (map-indexed
            (fn [idx e]
              ($ Email {:key (:id e) :email e :index idx & props}))
            emails)))
      (d/div
        {:class "rounded-md bg-green-200 p-4"}
        (d/h3 {:class "text-sm leading-5 font-medium text-green-800"} "No emails.")))))


(def EmailsList (with-keechma EmailsListRenderer))