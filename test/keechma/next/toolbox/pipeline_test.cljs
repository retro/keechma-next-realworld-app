(ns keechma.next.toolbox.pipeline-test
  (:require [cljs.test :refer-macros [deftest testing is async use-fixtures]]
            [keechma.next.toolbox.pipeline :as pp :refer [pswap! preset!] :refer-macros [pipeline!]]
            [promesa.core :as p]
            [cljs.core.async :refer [<! >! chan close! put! alts! timeout]])
  (:require-macros [cljs.core.async.macros :refer [go alt!]]))

(use-fixtures :once
              {:before (fn [] (js/console.clear))})

(defn delay-pipeline
  ([] (delay-pipeline 10))
  ([msec] (p/delay msec)))

(defn fn-returning-value []
  {:baz :qux})

(defn fn-returning-nil []
  nil)

(defn is-returning-nil [check]
  (is check)
  nil)

(defn fn-throwing []
  (throw (ex-info "ERROR" {:ex :info})))

(def promise-error (js/Error. "Promise Rejected"))

(defn fn-promise-rejecting []
  (p/create (fn [_ reject]
              (reject promise-error))))

(defn throwing-fn [msg]
  (throw (ex-info msg {})))

(defn make-context []
  {:log$ (atom [])
   :state$ (atom nil)})

(deftest basic-restartable-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (p/delay 250)
                                (pswap! state$ conj value))
                              (pp/restartable))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query "S")
        (<! (timeout 20))
        (invoke :query "SE")
        (<! (timeout 20))
        (invoke :query "SEA")
        (<! (timeout 20))
        (invoke :query "SEAR")
        (invoke :query "SEARC")
        (<! (timeout 20))
        (->> (invoke :query "SEARCH")
             (p/map (fn [_]
                      (is (= ["SEARCH"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest multiconcurrency-restartable-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (p/delay 250)
                                (pswap! state$ #(vec (conj % value))))
                              (pp/restartable 3))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (invoke :query 5)
        (<! (timeout 20))
        (->> (invoke :query 6)
             (p/map (fn [_]
                      (is (= [4 5 6] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest basic-dropping-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (p/delay 250)
                                (pswap! state$ #(vec (conj % value))))
                              (pp/dropping))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (invoke :query 5)
        (<! (timeout 20))
        (invoke :query 6)
        (<! (timeout 500))
        (is (= [1] @state$))
        (done)))))

(deftest multiconcurrency-dropping-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (p/delay 250)
                                (pswap! state$ #(vec (conj % value))))
                              (pp/dropping 3))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (invoke :query 5)
        (<! (timeout 20))
        (invoke :query 6)
        (<! (timeout 500))
        (is (= [1 2 3] @state$))
        (done)))))

(deftest basic-enqueued-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (pswap! state$ #(vec (conj % value)))
                                (p/delay 50)
                                (pswap! state$ #(vec (conj % (str "DONE-" value)))))
                              (pp/enqueued))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query "FIRST")
        (<! (timeout 20))
        (invoke :query "SECOND")
        (<! (timeout 20))
        (invoke :query "THIRD")
        (<! (timeout 20))
        (invoke :query "FOURTH")
        (invoke :query "FIFTH")
        (<! (timeout 20))
        (->> (invoke :query "SIXTH")
             (p/map (fn [_]
                      (is (= ["FIRST" "DONE-FIRST"
                              "SECOND" "DONE-SECOND"
                              "THIRD" "DONE-THIRD"
                              "FOURTH" "DONE-FOURTH"
                              "FIFTH" "DONE-FIFTH"
                              "SIXTH" "DONE-SIXTH"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest multiconcurrency-enqueued-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (pswap! state$ #(vec (conj % value)))
                                (p/delay 50)
                                (pswap! state$ #(vec (conj % (str "DONE-" value)))))
                              (pp/enqueued 3))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query "FIRST")
        (<! (timeout 20))
        (invoke :query "SECOND")
        (<! (timeout 20))
        (invoke :query "THIRD")
        (<! (timeout 20))
        (invoke :query "FOURTH")
        (invoke :query "FIFTH")
        (<! (timeout 20))
        (->> (invoke :query "SIXTH")
             (p/map (fn [_]
                      (is (= ["FIRST"
                              "SECOND"
                              "THIRD"
                              "DONE-FIRST"
                              "FOURTH"
                              "DONE-SECOND"
                              "FIFTH"
                              "DONE-THIRD"
                              "SIXTH"
                              "DONE-FOURTH"
                              "DONE-FIFTH"
                              "DONE-SIXTH"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))


(deftest set-queue-name-pipeline
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query-1 (-> (pipeline! [value {:keys [state$]}]
                                  (pswap! state$ #(vec (conj % value)))
                                  (p/delay 50)
                                  (pswap! state$ #(vec (conj % (str "DONE-" value)))))
                                (pp/enqueued)
                                (pp/set-queue :query))
                   :query-2 (-> (pipeline! [value {:keys [state$]}]
                                  (pswap! state$ #(vec (conj % value)))
                                  (p/delay 50)
                                  (pswap! state$ #(vec (conj % (str "DONE-" value)))))
                                (pp/enqueued)
                                (pp/set-queue :query))
                   :query-3 (-> (pipeline! [value {:keys [state$]}]
                                  (pswap! state$ #(vec (conj % value)))
                                  (p/delay 50)
                                  (pswap! state$ #(vec (conj % (str "DONE-" value)))))
                                (pp/enqueued)
                                (pp/set-queue :query))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query-1 "FIRST")
        (<! (timeout 20))
        (invoke :query-2 "SECOND")
        (<! (timeout 20))
        (invoke :query-3 "THIRD")
        (<! (timeout 20))
        (invoke :query-1 "FOURTH")
        (invoke :query-2 "FIFTH")
        (<! (timeout 20))
        (->> (invoke :query-3 "SIXTH")
             (p/map (fn [_]
                      (is (= ["FIRST" "DONE-FIRST"
                              "SECOND" "DONE-SECOND"
                              "THIRD" "DONE-THIRD"
                              "FOURTH" "DONE-FOURTH"
                              "FIFTH" "DONE-FIFTH"
                              "SIXTH" "DONE-SIXTH"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest set-queue-fn-pipeline
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:favorite (-> (pipeline! [value {:keys [state$]}]
                                   (pswap! state$ #(vec (conj % {:user (:user value)})))
                                   (p/delay 50)
                                   (pswap! state$ #(vec (conj % (str "DONE-" (:user value))))))
                                 (pp/enqueued)
                                 (pp/set-queue (fn [value]
                                                 [:user (:user value)])))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :favorite {:user 1})
        (<! (timeout 10))
        (invoke :favorite {:user 2})
        (<! (timeout 10))
        (invoke :favorite {:user 3})
        (<! (timeout 20))
        (invoke :favorite {:user 1})
        (invoke :favorite {:user 2})
        (->> (invoke :favorite {:user 3})
             (p/map (fn [_]
                      (is (= [{:user 1}
                              {:user 2}
                              {:user 3}
                              "DONE-1"
                              {:user 1}
                              "DONE-2"
                              {:user 2}
                              "DONE-3"
                              {:user 3}
                              "DONE-1"
                              "DONE-2"
                              "DONE-3"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest basic-keep-latest-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (p/delay 250)
                                (pswap! state$ #(vec (conj % value))))
                              (pp/keep-latest))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (invoke :query 5)
        (<! (timeout 20))
        (invoke :query 6)
        (<! (timeout 500))
        (is (= [1 6] @state$))
        (done)))))

(deftest multiconcurrency-keep-latest-pipeline []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (p/delay 250)
                                (pswap! state$ #(vec (conj % value))))
                              (pp/keep-latest 3))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (invoke :query 5)
        (<! (timeout 20))
        (invoke :query 6)
        (<! (timeout 250))
        (invoke :query 7)
        (<! (timeout 20))
        (invoke :query 8)
        (<! (timeout 20))
        (invoke :query 9)
        (<! (timeout 20))
        (invoke :query 10)
        (<! (timeout 500))
        (is (= [1 2 3 6 7 8 10] @state$))
        (done)))))

(deftest nested-restartable []
  (let [{:keys [state$] :as context} (make-context)
        restartable-pp (-> (pipeline! [value {:keys [state$]}]
                             (p/delay 100)
                             (pswap! state$ #(vec (conj % (str value "-END")))))
                           (pp/restartable))
        pipelines {:query (pipeline! [value {:keys [state$]}]
                            (pswap! state$ #(vec (conj % value)))
                            restartable-pp)}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (<! (timeout 20))
        (->> (invoke :query 5)
             (p/map (fn [_]
                      (is (= [1 2 3 4 5 "5-END"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest nested-enqueued []
  (let [{:keys [state$] :as context} (make-context)
        enqueued-pp (-> (pipeline! [value {:keys [state$]}]
                          (p/delay 100)
                          (pswap! state$ #(vec (conj % (str value "-END")))))
                        (pp/enqueued))
        pipelines {:query (pipeline! [value {:keys [state$]}]
                            (pswap! state$ #(vec (conj % value)))
                            enqueued-pp)}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (<! (timeout 20))
        (->> (invoke :query 5)
             (p/map (fn [_]
                      (is (= [1 2 3 4 5 "1-END" "2-END" "3-END" "4-END" "5-END"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))


(deftest nested-dropping []
  (let [{:keys [state$] :as context} (make-context)
        dropping-pp (-> (pipeline! [value {:keys [state$]}]
                          (p/delay 100)
                          (pswap! state$ #(vec (conj % (str value "-END")))))
                        (pp/dropping))
        pipelines {:query (pipeline! [value {:keys [state$]}]
                            (pswap! state$ #(vec (conj % value)))
                            dropping-pp)}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (<! (timeout 20))
        (invoke :query 5)
        (<! (timeout 200))
        (is (= [1 2 3 4 5 "1-END"] @state$))
        (done)))))

(deftest nested-keep-latest []
  (let [{:keys [state$] :as context} (make-context)
        keep-latest-pp (-> (pipeline! [value {:keys [state$]}]
                             (p/delay 100)
                             (pswap! state$ #(vec (conj % (str value "-END")))))
                           (pp/keep-latest))
        pipelines {:query (pipeline! [value {:keys [state$]}]
                            (pswap! state$ #(vec (conj % value)))
                            keep-latest-pp)}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query 1)
        (<! (timeout 20))
        (invoke :query 2)
        (<! (timeout 20))
        (invoke :query 3)
        (<! (timeout 20))
        (invoke :query 4)
        (<! (timeout 20))
        (invoke :query 5)
        (<! (timeout 200))
        (is (= [1 2 3 4 5 "1-END" "5-END"] @state$))
        (done)))))

(deftest shutting-down-runtime-cancels-live-pipelines []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (pipeline! [value {:keys [state$]}]
                            (p/delay 100)
                            (pswap! state$ #(vec (conj % value))))}
        {:keys [invoke shutdown-runtime]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query :query-1)
        (invoke :query :query-2)
        (invoke :query :query-3)
        (shutdown-runtime)
        (<! (timeout 200))
        (is (= nil @state$))
        (done)))))

(deftest pipeline-can-be-configured-to-survive-shutdown []
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (pipeline! [value {:keys [state$]}]
                            (p/delay 100)
                            (pswap! state$ #(vec (conj % value))))
                   :surviving-query (-> (pipeline! [value {:keys [state$]}]
                                          (p/delay 100)
                                          (pswap! state$ #(vec (conj % [:surviving-query value]))))
                                        (pp/cancel-on-shutdown false))}
        {:keys [invoke shutdown-runtime]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query :query-1)
        (invoke :query :query-2)
        (invoke :query :query-3)
        (invoke :surviving-query "I will survive")
        (shutdown-runtime)
        (<! (timeout 200))
        (is (= [[:surviving-query "I will survive"]] @state$))
        (done)))))

(deftest use-existing-pipeline
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:query (-> (pipeline! [value {:keys [state$]}]
                                (pswap! state$ #(vec (conj % value)))
                                (p/delay 50)
                                (pswap! state$ #(vec (conj % (str "DONE-" value)))))
                              (pp/use-existing))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (invoke :query "FIRST")
        (<! (timeout 10))
        (->> (invoke :query "FIRST")
             (p/map (fn [_]
                      (is (= ["FIRST" "DONE-FIRST"] @state$))
                      (done)))
             (p/error (fn [_]
                        (is false)
                        (done))))))))

(deftest sync-behavior-1
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:inc (pipeline! [value {:keys [state$]}]
                          (pswap! state$ inc)
                          (pswap! state$ inc)
                          (pswap! state$ inc))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :inc)
    (is (= 3 @state$))))

(deftest sync-behavior-2
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:inc (pipeline! [value {:keys [state$]}]
                          (inc value)
                          (inc value)
                          (inc value)
                          (preset! state$ value))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :inc 1)
    (is (= 4 @state$))))

(deftest errors-1
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:inc (pipeline! [value {:keys [state$]}]
                          (pswap! state$ inc)
                          (throw (ex-info "Error" {:error true}))
                          (rescue! [error]
                                   (is (= (ex-message error) "Error"))
                                   (is (= (ex-data error) {:error true}))
                                   (preset! state$ [@state$ :rescued])))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :inc)
    (is (= [1 :rescued] @state$))))

(deftest errors-2
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:inc (pipeline! [value {:keys [state$]}]
                          (pswap! state$ inc)
                          (p/delay 20)
                          (throw (ex-info "Error" {:error true}))
                          (rescue! [error]
                                   (is (= (ex-message error) "Error"))
                                   (is (= (ex-data error) {:error true}))
                                   (preset! state$ [@state$ :rescued])))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (is (nil? @state$))
        (invoke :inc)
        (<! (timeout 20))
        (is (= [1 :rescued] @state$))
        (done)))))

(deftest sending-promise-as-value
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:inc (pipeline! [value {:keys [state$]}]
                          (inc value)
                          (inc value)
                          (inc value)
                          (preset! state$ value))}
        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (is (nil? @state$))
      (->> (invoke :inc (p/promise 1))
           (p/map (fn [value]
                    (is (= 4 value @state$))
                    (done)))))))

(deftest finally-1
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:run (pipeline! [value {:keys [state$]}]
                          [:begin]
                          (finally! [error]
                                   (conj value :finally)
                                   (preset! state$ value)))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :run)
    (is (= [:begin :finally] @state$))))

(deftest finally-2
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:run (pipeline! [value {:keys [state$]}]
                          [:begin]
                          (throw (ex-info "FOOBAR" {}))
                          (finally! [error]
                                    (conj value :finally)
                                    (preset! state$ value)
                                    (is (= "FOOBAR" (ex-message error)))))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :run)
    (is (= [:begin :finally] @state$))))

(deftest finally-3
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:run (pipeline! [value {:keys [state$]}]
                          [:begin]
                          (throw (ex-info "FOOBAR" {}))
                          (rescue! [error]
                                   (conj value :rescue))
                          (finally! [error]
                                    (conj value :finally)
                                    (preset! state$ value)
                                    (is (= "FOOBAR" (ex-message error)))))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :run)
    (is (= [:begin :rescue :finally] @state$))))

(deftest finally-4
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:run (pipeline! [value {:keys [state$]}]
                          [:begin]
                          (throw (ex-info "FOOBAR" {}))
                          (rescue! [error]
                                   (conj value :rescue)
                                   (throw (ex-info "BARBAZ" {})))
                          (finally! [error]
                                    (conj value :finally)
                                    (preset! state$ value)
                                    (is (= "BARBAZ" (ex-message error)))))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :run)
    (is (= [:begin :rescue :finally] @state$))))

(deftest rescue-1
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:run (pipeline! [value {:keys [state$]}]
                          [:begin]
                          (throw (ex-info "FOOBAR" {}))
                          (rescue! [error]
                                    (conj value :rescue)
                                    (preset! state$ value)
                                    (is (= "FOOBAR" (ex-message error)))))}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (is (nil? @state$))
    (invoke :run)
    (is (= [:begin :rescue] @state$))))

(deftest detachable-pipeline-1
  (let [{:keys [state$] :as context} (make-context)
        pipelines {:run (-> (pipeline! [value {:keys [state$]}]
                              (preset! state$ [[value :main-1]])
                              (pp/detach-pipeline
                                (pipeline! [value {:keys [state$]}]
                                  (pswap! state$ conj [value :detached-1])
                                  (p/delay 10)
                                  (pswap! state$ conj [value :detached-2])
                                  (p/delay 50)
                                  (pswap! state$ conj [value :detached-3])))
                              (pswap! state$ conj [value :main-2])
                              (p/delay 100))
                            pp/restartable)}

        {:keys [invoke]} (pp/make-runtime context pipelines)]
    (async done
      (go
        (is (nil? @state$))
        (invoke :run 1)
        (is (= [[1 :main-1]
                [1 :detached-1]
                [1 :main-2]] @state$))
        (<! (timeout 30))
        (is (= [[1 :main-1]
                [1 :detached-1]
                [1 :main-2]
                [1 :detached-2]
                ] @state$))
        (->> (invoke :run 2)
             (p/map (fn []
                      (is (= [[2 :main-1]
                              [2 :detached-1]
                              [2 :main-2]
                              [2 :detached-2]
                              [2 :detached-3]] @state$))
                      (done))))))))