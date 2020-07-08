(ns workshop.ui.main
  (:require [keechma.next.helix.core :refer [with-keechma use-sub]]
            [keechma.next.helix.lib :refer [defnc]]
            [keechma.next.helix.template :refer [defnt fill-slot configure]]
            [keechma.next.helix.classified :refer [defclassified]]
            [helix.core :as hx :refer [$ <>]]
            [helix.hooks :as hooks]
            ["react" :as react]
            ["react-dom" :as rdom]
            [helix.dom :as d]
            [keechma.next.controllers.router :as router]
            [workshop.ui.components.emails-list :refer [EmailsList]]
            [workshop.ui.components.email-thread :refer [EmailThread]]
            [workshop.ui.components.composer :refer [Composer]]
            [dv.cljs-emotion :refer [defstyled keyframes global-style theme-provider]]))

(def links
  [{:route {:page "inbox"}
    :title "Inbox"}
   {:route {:page "important"}
    :title "Important"}
   {:route {:page "sent"}
    :title "Sent"}])

(defnc Link [{:keys [link] :as props}]
  (let [route (use-sub props :router)]
    (d/a
      {:class ["mt-1 group flex items-center px-2 py-2 text-sm leading-5 font-medium text-indigo-300 rounded-md hover:text-white hover:bg-indigo-700 focus:outline-none focus:text-white focus:bg-indigo-700 transition ease-in-out duration-150" (when (= (get-in link [:route :page]) (:page route)) "bg-indigo-700")]
       :href (router/get-url props :router (:route link))}
      (:title link))))

(defnt TemplatedInner [props]
  (d/div {:class "p-2 border m-2 border-indigo-700"}
    "I'm templated inner"
    (d/br)
    (d/slot :main "Here's my main slot")))

(defnt Templated [props]
  (d/div
    (slot :main "I'm templated " "Some more templated content"
      (d/div (configurable :wrapper {:class "bg-indigo-800"})
        (slot :main/foo "I'm inside")))
    ($ TemplatedInner (configurable :inner))
    (map
      (fn [idx]
        (d/div {:key idx} (slot :in-loop idx)))
      (range 0 5))
    (optional-slot :optional
      (d/div "I'm optional"
        (d/div
          (slot :optional/inner))))))


(defstyled HeadingBase :h1
  {:background-color "yellow"})

(defclassified Heading HeadingBase
  "border border-gray-600 p-2"
  (fn [{:keys [count]}] (when (pos? count) "m-4 text-red-500 font-bold"))
  (fn [{:keys [count]}] (when (zero? count) "text-xl")))

(defclassified Heading2 :h2
  "border border-red-500 p-4")

(defstyled h2 Heading2
  {:background-color "cyan"})

(defnc MainRenderer [props]
  (let [router (use-sub props :router)
        [counter set-counter] (hooks/use-state 0)
        ref (hooks/use-ref nil)]
    (<>
      (d/div
        {:class "h-screen flex overflow-hidden bg-gray-100"}
        (d/div
          {:class "flex flex-shrink"}
          (d/div
            {:class "flex flex-col w-64 border-r border-gray-200 bg-indigo-800"}
            (d/nav
              {:class "mt-5 flex-1 px-2 bg-indigo-800"}
              (map
                (fn [l]
                  ($ Link {:key (:title l) :link l & props}))
                links))))
        (d/div
          {:class "flex flex-row w-0 flex-1 overflow-hidden"}
          (d/main
            {:class "flex-1 relative z-0 overflow-y-auto focus:outline-none py-6 px-4"}
            (d/button {:on-click #(set-counter inc)} "INC")
            (h2 "H2")
            ($ Heading {:count counter} "Here's a heading")
            ($ Templated {& (-> {}
                              (fill-slot :main/foo "Here's another")
                              (fill-slot :optional/inner "Hello from inner")
                              (fill-slot :main (fn [super] (<> (super) (d/br) "I'm from the outside" (super))))
                              (fill-slot :in-loop (fn [super] (<> (super) "---> Hello")))
                              (configure :wrapper {:class "bg-indigo-300 p-4"})
                              (configure :inner #(fill-slot % :main (str "Hello content from outside " counter))))})
            ($ EmailsList))
          (when (:id router)
            (d/aside
              {:class "flex-1 relative z-0 overflow-y-auto focus:outline-none py-6 px-4"}
              ($ EmailThread)))))
      (d/div
        {:class "fixed bottom-0 right-0 mr-32 bg-white shadow-lg p-4"
         :style {:width "400px"}}
        ($ Composer)))))

(def Main (with-keechma MainRenderer))