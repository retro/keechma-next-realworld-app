(ns keechma.next.toolbox.dataloader-test
  (:require [cljs.test :refer-macros [deftest testing is async use-fixtures]]
            [keechma.next.toolbox.dataloader :as dl]
            [promesa.core :as p] 
            [keechma.next.toolbox.dataloader.request-manager :as rm]
            [keechma.next.toolbox.protocols :as pt]
            [keechma.next.toolbox.ajax :refer [GET]])
  (:require-macros [cljs.core.async.macros :as m :refer [go alt!]]))

(use-fixtures :once
  {:before (fn [] (js/console.clear))})


(defn jwt-loader []
  (js/console.count "JWT-LOADER")
  (p/delay 1 "JWT"))
(defn board-loader []
  (js/console.count "BOARD-LOADER")
  (specify! (p/delay 100 [{:board/id 1} {:board/id 2}])
    pt/IAbortable
    (abort! [this]
      (p/reject! this (ex-info "AAA" {})))))
(defn lists-loader []
  (p/delay 1 [{:list/id 1} {:list/id 2} {:list/id 3} {:list/id 4}])
  ;;(p/rejected (ex-info "EERRROR" {}))
  )
(defn tasks-loader [params]
  (p/delay (* 100 (get-in params [:variables :list-id])) [{:task/id [(get-in params [:variables :list-id]) 1]}]))
(defn gql-loader [params]
  (p/delay 100 {:foo :bar :parent-id (get-in params [:variables :id])}))
(defn fleet-loader []
  (p/delay 100 {:fleet/id 1 :vessels [{:id 1} {:id 2} {:id 3}]}))

(defn get-loader [params]
  (GET (:url params) {:keywords? true :response-format :json}))

(defn fn-node []
  [:foo {:bar :baz}])

(defn datasources [params resolved]
  (js/console.count "DS")
  [:<>
   [:jwt (dl/request jwt-loader)]
   (when-let [jwt (:jwt resolved)]
     [:<>
      [:boards (dl/request board-loader {:variables {:id (:board/id params)} :token jwt})
       (map
        (fn [b]
          [:lists (dl/request lists-loader {:variables {:board-id (:board/id params) :token jwt}}) 
           (map
            (fn [l]
              [:tasks (dl/request tasks-loader {:variables {:list-id (:list/id l)} :token jwt})])
            (:lists b))])
        (:boards resolved))]
      [:fleet (dl/request fleet-loader)
       [:vessels
        (map 
         (fn [vessel]
           [:account (dl/request gql-loader {:query :vessel-account :variables {:id (:id vessel)} :token jwt}) ]) 
         (get-in resolved [:fleet :vessels]))]]])])

(defn sw-datasources [params resolved]
  [:<>
   [:luke (dl/request get-loader {:url "https://swapi.co/api/people/1/"})
    [:films
     (map 
      (fn [f]
        (dl/request get-loader {:url f}))
      (get-in resolved [:luke :films]))]]])

#_(deftest dataloader []
  (let [request-manager (rm/constructor)]
    (async done
           (let [req (dl/resolve datasources {} {} {:dataloader/request-manager request-manager})]
             (-> req
                 (p/finally (fn [res err]
                              (js/console.log "->" err)
                              (js/console.log (with-out-str (cljs.pprint/pprint res)))
                              (is (= res {:jwt "JWT",
                                          :boards
                                          [{:board/id 1,
                                            :lists
                                            [{:list/id 1, :tasks [{:task/id [1 1]}]}
                                             {:list/id 2, :tasks [{:task/id [2 1]}]}
                                             {:list/id 3, :tasks [{:task/id [3 1]}]}
                                             {:list/id 4, :tasks [{:task/id [4 1]}]}]}
                                           {:board/id 2,
                                            :lists
                                            [{:list/id 1, :tasks [{:task/id [1 1]}]}
                                             {:list/id 2, :tasks [{:task/id [2 1]}]}
                                             {:list/id 3, :tasks [{:task/id [3 1]}]}
                                             {:list/id 4, :tasks [{:task/id [4 1]}]}]}],
                                          :fleet
                                          {:fleet/id 1,
                                           :vessels
                                           [{:id 1, :account {:foo :bar, :parent-id 1}}
                                            {:id 2, :account {:foo :bar, :parent-id 2}}
                                            {:id 3, :account {:foo :bar, :parent-id 3}}]}}))
                              (println (with-out-str (cljs.pprint/pprint res)))
                              (rm/shutdown request-manager)
                              (done))))))))

