(ns router.core-test
  (:require [cljs.test :refer-macros [deftest is use-fixtures testing are]]
            [router.core :as router]
            [router.util :refer [encode-query-params decode-query-params]]))

#_(use-fixtures :once {:before (fn [] (js/console.clear))})

(deftest route->parts
  (is (= [":foo" "/" "bar" "/" ":baz"] (router/route->parts ":foo/bar/:baz")))
  (is (= [":foo/bar" "/" "baz" "/" ":bar.qux/foo" "/" ":qux/baz"] (router/route->parts "{:foo/bar}/baz/{:bar.qux/foo}/{qux/baz}")))
  (is (thrown? js/Error (router/route->parts "{:foo")))
  (is (thrown? js/Error (router/route->parts "{f:oo}"))))

(deftest match-path []
  (let [routes (router/expand-routes ["/:foo/bar/:baz"])
        matched-path (router/match-path routes "one/bar/two")]
    (is (= matched-path {:route ":foo/bar/:baz" :data {:foo "one" :baz "two"}}))))

(deftest url->map []
  (let [routes (router/expand-routes [[":page" {:page "index"}]])
        matched-1 (router/url->map routes "foo.Bar")
        matched-2 (router/url->map routes "")
        matched-3 (router/url->map routes "foo.Bar?where=there")]
    (is (= matched-1 {:route ":page"
                      :data {:page "foo.Bar"}}))
    (is (= matched-2 {:route ":page"
                      :data {:page "index"}}))
    (is (= matched-3 {:route ":page"
                      :data {:page "foo.Bar"
                             :where "there"}})))

  (let [routes (router/expand-routes [[":page/:bar" {:page "index" :bar "foo"}]])
        matched (router/url->map routes "foo.Bar/?where=there")]
    (is (= matched {:route ":page/:bar"
                    :data {:page "foo.Bar"
                           :bar "foo"
                           :where "there"}})))

  (let [routes (router/expand-routes ["/:foo/bar/:baz"])
        url "/one/bar/two?qux=1&test=success"
        matched (router/url->map routes url)
        expected-data {:foo "one" :baz "two" :qux "1" :test "success"}]
    (is (= matched {:route ":foo/bar/:baz"
                    :data expected-data})))
  (let [routes (router/expand-routes [":page"])]
    (is (= (router/url->map routes "Hello%20World")
           {:route ":page"
            :data {:page "Hello World"}})))
  (let [routes (router/expand-routes [":folder/:file.:ext"])]
    (is (= {:route ":folder/:file.:ext" :data {:folder "desktop" :file "image", :ext "jpg"}}
           (router/url->map routes "desktop/image.jpg")))))

(deftest url->map-invalid []
  (let [routes (router/expand-routes [["pages/:var1/:var2/:var3"
                                       {:var1 "default1"
                                        :var2 "default2"
                                        :var3 "default3"}]])
        matched-1 (router/url->map routes "pages//")
        matched-2 (router/url->map routes "pages/val1/val2/val3?invalid-param")]
    (is (= matched-1 {:data {}}))
    (is (= matched-2 {:route "pages/:var1/:var2/:var3"
                      :data {:var1 "val1"
                             :var2 "val2"
                             :var3 "val3"}}))))

(deftest url->map-only-query-string []
  (let [routes []
        url "?foo=bar&baz=qux"
        matched-url (router/url->map routes url)]
    (is (= matched-url {:data {:foo "bar" :baz "qux"}}))))

(deftest map->url []
  (let [routes (router/expand-routes [["pages/:page" {:page "index"}]])
        url-1 (router/map->url routes {:page "foo"})
        url-2 (router/map->url routes {:page "foo" :index "bar"})]
    (is (= url-1 "pages/foo"))
    (is (= url-2 "pages/foo?index=bar")))
  (let [routes (router/expand-routes [["pages/:page" {:page "index"}]
                                      ["pages/:page/:foo" {:page "index" :foo "bar"}]])
        url (router/map->url routes {:page "foo" :foo "bar" :where "there"})]
    (is (= url "pages/foo/?where=there")))
  (let [url (router/map->url nil {:page "foo" :bar "baz" :where "there"})]
    (is (= url "?page=foo&bar=baz&where=there"))))

(deftest symmetry []
  (let [data {:page "=&[]" :nestedArray ["a"] :nested {:a "b"}}
        url (router/map->url [] data)
        back-data (router/url->map [] url)]
    (is (= data (:data back-data)))))

(deftest light-param []
  (let [routes (router/expand-routes [[":page", {:page "index"}]])
        res (router/map->url routes {:page "index"})]
    (is (= res "")))
  (let [routes (router/expand-routes [["pages/:p1/:p2/:p3" {:p1 "index" :p2 "foo" :p3 "bar"}]])
        res-1 (router/map->url routes {:p1 "index" :p2 "foo" :p3 "bar"})
        res-2 (router/map->url routes {:p1 "index" :p2 "baz" :p3 "bar"})]
    (is (= res-1 "pages///"))
    (is (= res-2 "pages//baz/"))))

(deftest map->url-does-not-add-defaults []
  (let [routes (router/expand-routes [["pages/:p1", {:p2 "foo"}]])]
    (is (= (router/map->url routes {:p1 "index" :p2 "foo"}) "pages/index"))))

(deftest map->url-url->map []
  (let [routes (router/expand-routes [
                                      [":page/:type", {:page "index", :type "foo"}]])
        data {:page "foo.Bar" :type "document" :bar "baz" :where "there"}
        url (router/map->url routes data)
        url-data (router/url->map routes url)
        data-2 {:page "foo.Bar" :type "foo" :bar "baz" :where "there"}
        url-2 (router/map->url routes data-2)
        url-data-2 (router/url->map routes url-2)
        data-3 {:page "index" :type "foo" :bar "baz" :where "there"}
        url-3 (router/map->url routes data-3)
        url-data-3 (router/url->map routes url-3)]
    (is (= data (:data url-data)))
    (is (= data-2 (:data url-data-2)))
    (is (= data-3 (:data url-data-3))))
  (let [data {:page "foo" :bar "baz" :where "there"}
        url (router/map->url [] data)
        url-data (router/url->map [] url)]
    (is (= data (:data url-data))))
  (let [routes (router/expand-routes [[":foo/:bar" {:foo 1 :bar 2}]])
        url (router/map->url routes {:foo 1 :bar 2})
        data (router/url->map routes "/")]
    (is (= url ""))
    (is (= {:foo 1 :bar 2} (:data data)))))

(deftest precedence []
  (let [routes (router/expand-routes [[":who", {:who "index"}]
                                      "search/:search"])
        data-1 (router/url->map routes "foo.Bar")
        data-2 (router/url->map routes "search/foo.Bar")
        url-1 (router/map->url routes {:who "foo.Bar"})
        url-2 (router/map->url routes {:search "foo.Bar"})]
    (is (= (:data data-1) {:who "foo.Bar"}))
    (is (= (:data data-2) {:search "foo.Bar"}))
    (is (= url-1 "foo.Bar"))
    (is (= url-2 "search/foo.Bar")))
  (let [routes (router/expand-routes [[":type" , {:who "index"}]
                                      ":type/:id"])
        data (router/url->map routes "foo/bar")
        url (router/map->url routes {:type "foo" :id "bar"})]
    (is (= (:data data) {:type "foo" :id "bar"}))
    (is (= url "foo/bar"))))

(deftest namespaced-keys
  (let [routes (router/expand-routes [["" {:page "homepage"}]
                                      ":page"
                                      ["blog" {:page "blog-index" :blog-posts/page 1}]
                                      ["blog/p/{:blog-posts/page}" {:page "blog-index"}]
                                      ["blog/{:blog-post/slug}" {:page "blog-post"}]
                                      ["users/list" {:page "users"}]
                                      ["users/{:user/id}" {:page "user-details"}]
                                      ["users/{:user/id}/{:user/page}"]])]
    (is (= "" (router/map->url routes {:page "homepage"})))
    (is (= "foo" (router/map->url routes {:page "foo"})))
    (is (= "users/list" (router/map->url routes {:page "users"})))
    (is (= "users/1" (router/map->url routes {:user/id 1})))
    (is (= "users/1/details" (router/map->url routes {:user/id 1 :user/page "details"})))
    (is (= "?foo/bar=1&foo.bar/baz=2&foo.bar.baz/qux[qux/foo]=bar&foo.bar.baz.qux/foo[bar.baz/qux]=foo"
           (router/map->url routes {:foo/bar 1
                                    :foo.bar/baz 2
                                    :foo.bar.baz/qux {:qux/foo "bar"}
                                    :foo.bar.baz.qux/foo {:bar.baz/qux "foo"}})))

    (is (= {:page "homepage"} (:data (router/url->map routes ""))))
    (is (= {:page "homepage"} (:data (router/url->map routes "homepage"))))
    (is (= {:page "foo"} (:data (router/url->map routes "foo"))))
    (is (= {:page "users"} (:data (router/url->map routes "users/list"))))
    (is (= {:page "user-details" :user/id "1"} (:data (router/url->map routes "users/1"))))
    (is (= {:user/id "1" :user/page "details"} (:data (router/url->map routes "users/1/details"))))
    (is (= {:foo/bar "1"
            :foo.bar/baz "2"
            :foo.bar.baz/qux {:qux/foo "bar"}
            :foo.bar.baz.qux/foo {:bar.baz/qux "foo"}
            :page "homepage"}
           (:data (router/url->map routes "?foo/bar=1&foo.bar/baz=2&foo.bar.baz/qux[qux/foo]=bar&foo.bar.baz.qux/foo[bar.baz/qux]=foo"))))))

(deftest splat-routes
  (let [routes (router/expand-routes [["" {:page "homepage"}]
                                      ":page"
                                      "fs/{:folders/*}/:file.:ext"
                                      [":*" {:page "not-found"}]])]
    (is (= {:page "homepage"} (:data (router/url->map routes ""))))
    (is (= {:page "foo"} (:data (router/url->map routes "foo"))))
    (is (= {:page "not-found" :* "foo/bar/baz"} (:data (router/url->map routes "foo/bar/baz"))))
    (is (= {:folders/* "foo/bar/baz" :file "img" :ext "jpg"} (:data (router/url->map routes "fs/foo/bar/baz/img.jpg"))))
    (is (= "fs/foo/bar/baz/img.jpg" (router/map->url routes {:folders/* "foo/bar/baz" :file "img" :ext "jpg"})))
    (is (= "foo/bar" (router/map->url routes {:* "foo/bar"})))))

(deftest duplicate-placeholders
  (is (thrown? js/Error (router/expand-routes [":foo/bar/:foo"]))))

(deftest trailing-slash
  (let [routes (router/expand-routes [":page"])]
    (is (= {:page "homepage"} (:data (router/url->map routes "homepage/"))))))

(deftest query-params-test
  (testing "encodes query params"
           (let [params {:id "kevin" :food "bacon"}
                 encoded (encode-query-params params)]
             (is (= (decode-query-params encoded)
                    params)))

           (are [x y] (= (encode-query-params x) y)
                {:x [1 2]} "x[]=1&x[]=2"
                {:a [{:b 1} {:b 2}]} "a[0][b]=1&a[1][b]=2"
                {:a [{:b [1 2]} {:b [3 4]}]} "a[0][b][]=1&a[0][b][]=2&a[1][b][]=3&a[1][b][]=4"))

  (testing "decodes query params"
           (let [query-string "id=kevin&food=bacong"
                 decoded (decode-query-params query-string)
                 encoded (encode-query-params decoded)]
             (is (re-find #"id=kevin" query-string))
             (is (re-find #"food=bacon" query-string)))

           (are [x y] (= (decode-query-params x) y)
                "x[]=1&x[]=2" {:x ["1" "2"]}
                "a[0][b]=1&a[1][b]=2" {:a [{:b "1"} {:b "2"}]}
                "a[0][b][]=1&a[0][b][]=2&a[1][b][]=?3&a[1][b][]=4" {:a [{:b ["1" "2"]} {:b ["?3" "4"]}]})))

(defn mean [coll]
  (let [sum (apply + coll)
        count (count coll)]
    (if (pos? count)
      (/ sum count)
      0)))

(defn median [coll]
  (let [sorted (sort coll)
        cnt (count sorted)
        halfway (quot cnt 2)]
    (if (odd? cnt)
      (nth sorted halfway) ; (1)
      (let [bottom (dec halfway)
            bottom-val (nth sorted bottom)
            top-val (nth sorted halfway)]
        (mean [bottom-val top-val])))))

(defn standard-deviation [coll]
  (let [avg (mean coll)
        squares (for [x coll]
                  (let [x-avg (- x avg)]
                    (* x-avg x-avg)))
        total (count coll)]
    (-> (/ (apply + squares)
           (- total 1))
        (Math/sqrt))))

#_(deftest performance
  (let [routes (router/expand-routes [["" {:page "homepage"}]
                                      ":page"
                                      ["blog" {:page "blog-index" :blog-posts/page 1}]
                                      ["blog/p/{:blog-posts/page}" {:page "blog-index"}]
                                      ["blog/{:blog-post/slug}" {:page "blog-post"}]
                                      ["users/list" {:page "users"}]
                                      ["users/{:user/id}" {:user/page "user-details"}]
                                      ["users/{:user/id}/{:user/page}"]])
        measurements
        (map
          (fn [_]
            (let [t (js/performance.now)]
              (doseq [i (range 0 10000)]
                (let [user-page (if (even? i) "user-details" "feed")]
                  (router/map->url routes {:user/id i :user/page user-page})))
              (- (js/performance.now) t)))
          (range 0 10))]
    (println "Mean:" (mean measurements) "High:" (last (sort measurements)) "Low:" (first (sort measurements)))
    (println "Median:" (median measurements))
    (println "SD:" (standard-deviation measurements))))