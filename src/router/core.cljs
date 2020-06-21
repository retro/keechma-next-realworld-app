(ns router.core
  (:require [clojure.walk :refer [postwalk]]
            [clojure.set :refer [superset? union intersection]]
            [router.util :refer [decode-query-params encode-query-params]]
            [clojure.string :as str]))

(def ^:private encode js/encodeURIComponent)

(defn ^:private placeholder->key [p]
  (-> (subs p 1)
      keyword))

(defn ^:private process-route-part [default-keys part]
  (case part
    "/" {:part part :re-match (str "/")}
    "." {:part part :re-match (str "\\.")}
    (let [is-placeholder (= ":" (first part))
          key (when is-placeholder (placeholder->key part))
          has-default (contains? default-keys key)
          min-matches (if has-default "*" "+")
          re-match (if is-placeholder (str "(" "[^/]" min-matches ")") part)]
      {:is-placeholder is-placeholder
       :key key
       :part part
       :has-default has-default
       :re-match re-match})))

(defn ^:private route-regex [parts]
  (let [base-regex (str/join (map (fn [p] (:re-match p)) parts))
        full-regex (str "^" base-regex "$")]
    (re-pattern full-regex)))

(defn ^:private route-placeholders [parts]
  (->> parts
       (map :key)
       (remove nil?)))

(defn ^:private add-default-params [route]
  (if (vector? route) route [route {}]))

(defn ^:private strip-slashes
  ([route]
   (str/replace (str/trim (str route)) #"^/+|/+$" ""))
  ([side route]
   (case side
     :left (str/replace (str/trim (or route "")) #"^/+" "")
     :right (str/replace (str/trim (or route "")) #"/+$" "")
     route)))

(defn consume-colon-placeholder [r]
  (loop [route r
         part []]
    (if-not (seq route)
      {:part (str/join part) :rest-route nil}
      (let [[c & rest-route] route]
        (cond
          (contains? #{"." "{" "/" ":"} c)
          {:part (str/join part) :rest-route route}

          (or (= "}" c))
          (throw (ex-info "Mismatched braces" {::route r}))

          :else
          (recur rest-route (conj part c)))))))

(defn consume-brace-placeholder [r]
  (loop [route r
         part []]
    (if-not (seq route)
      (throw (ex-info "Missing closing brace" {::route r}))
      (let [[c & rest-route] route]
        (cond
          (and (= ":" c) (nil? (seq part)))
          (recur rest-route part)

          (= ":" c)
          (throw (ex-info "Colon must be a first character in the placeholder" {::route r}))

          (= "}" c)
          {:part (str/join part) :rest-route rest-route}

          :else
          (recur rest-route (conj part c)))))))

(defn consume-static [r]
  (loop [route r
         part []]
    (if-not (seq route)
      {:part (str/join part) :rest-route nil}
      (let [[c & rest-route] route]
        (if (contains? #{"." "{" ":" "/"} c)
          {:part (str/join part) :rest-route route}
          (recur rest-route (conj part c)))))))

(defn route->parts [route]
  (loop [parts []
         rest-route route]
    (if-not (seq rest-route)
      parts
      (let [[c & rest-route'] rest-route]
        (cond
          (contains? #{"." "/"} c)
          (recur (conj parts c) rest-route')

          (= ":" c)
          (let [{:keys [part rest-route]} (consume-colon-placeholder rest-route')]
            (recur (conj parts (str ":" part)) rest-route))

          (= "{" c)
          (let [{:keys [part rest-route]} (consume-brace-placeholder rest-route')]
            (recur (conj parts (str ":" part)) rest-route))

          :else
          (let [{:keys [part rest-route]} (consume-static rest-route)]
            (recur (conj parts part) rest-route)))))))

(defn ^:private process-route [[route defaults]]
  (if (= :* route)
    {:parts []
     :regex #".*"
     :placeholders #{}
     :route route
     :defaults (or defaults {})
     :specificity 0
     :type ::catch-all}
    (let [parts (route->parts route)
          processed-parts (map (partial process-route-part (set (keys defaults))) parts)
          placeholders  (set (route-placeholders processed-parts))]
      {:parts processed-parts
       :regex (route-regex processed-parts)
       :placeholders placeholders
       :route route
       :defaults (or defaults {})
       :specificity (+ (count placeholders) (* 1.001 (count defaults)))
       :type (if (empty? placeholders) ::exact ::pattern)})))

(defn ^:private remove-empty-matches [matches]
  (->> matches
       (filter (fn [[k v]]
                 (and (not (nil? v))
                      (not (nil? k))
                      (not (empty? v))
                      (not= "null" v))))
       (into {})))

(defn ^:private expand-route [route]
  (let [strip-slashes (fn [[route defaults]] [(if (string? route) (strip-slashes route) route) defaults])]
    (-> route
        add-default-params
        strip-slashes
        process-route)))

(defn ^:private match-path-with-route [route url]
  (let [matches (first (re-seq (:regex route) url))]
    (when-not (nil? matches)
      (zipmap (:placeholders route) (map js/decodeURIComponent (rest matches))))))

(defn ^:private match-path [expanded-routes path]
  (reduce
    (fn [result route]
      (when-let [matches (match-path-with-route route path)]
        (reduced {:route (:route route)
                  :data  (merge (:defaults route) (remove-empty-matches matches))})))
    nil
    expanded-routes))


(defn ^:private intersect-maps [map1 map2]
  (reduce-kv (fn [m k v]
               (if (= (get map2 k) v)
                 (assoc m k v)
                 m)) {} map1))

(defn ^:private get-url-segment [defaults data k]
  (let [val (get data k)
        is-default (= (get defaults k) val)]
    (if is-default "" (encode val))))

(defn dissoc-defaults [data defaults]
  (reduce-kv
    (fn [acc k v]
      (if (= (get acc k) v)
        (dissoc acc k)
        acc))
    data
    defaults))

(defn build-url [route data]
  (let [defaults (:defaults route)
        {:keys [query-params base-url]}
        (loop [parts (:parts route)
               data data
               defaults defaults
               url []]
          (if (not (seq parts))
            {:base-url (str/join url)
             :query-params (dissoc-defaults data defaults)}
            (let [[part & rest-parts] parts]
              (if-let [key (:key part)]
                (let [rest-data (dissoc data key)
                      rest-defaults (dissoc defaults key)
                      url-segment (get-url-segment defaults data key)]
                  (recur rest-parts rest-data rest-defaults (conj url url-segment)))
                (recur rest-parts data defaults (conj url (:part part)))))))]
    (if (empty? query-params)
      (if (= "/" base-url) "" base-url)
      (str base-url "?" (encode-query-params query-params)))))

(defn ^:private route-score [data {:keys [placeholders defaults] :as route}]
  (loop [placeholders-score 0
         defaults-score 0 
         data-keys (union placeholders (set (keys defaults)))]
    (if-not (seq data-keys)
      (+ placeholders-score defaults-score)
      (let [[k & rest-data-keys] data-keys
            datum (get data k)]
        (recur
          (if (and datum (contains? placeholders k)) (inc placeholders-score) placeholders-score)
          (if (and datum (= (get data k) (get defaults k))) (+ 1.1 defaults-score) defaults-score)
          rest-data-keys)))))

(defn get-best-route [expanded-routes data]
  (loop [best-route nil
         best-score 0
         routes expanded-routes]
    (if-not (seq routes)
      best-route
      (let [[route & rest-routes] routes
            score (route-score data route)]
        (if (< best-score score)
          (recur route score rest-routes)
          (recur best-route best-score rest-routes))))))


;; Public API

(defn url->map
  "Accepts `expanded-routes` vector (returned by the `expand-routes` function)
  and a string as arguments. Returns a map which contains the data represented
  by the route.

  ```clojure
  ;; define routes
  (def routes [[\":page\", {:page \"index\"}]
                \":page/:id\"
                \":page/:id/:action\"])

  (def expanded-routes (expand-routes routes))

  (url->map expanded-routes \"foo\")
  ;; {:page \"foo\"}

  (url->map expanded-routes \"foo/1\")
  ;; {:page \"foo\" :id 1}

  (url->map expanded-routes \"foo?bar=baz\")
  ;; {:page \"foo\" :bar \"baz\"}
  ```
  "
  [expanded-routes url]
  (let [[u q] (str/split url #"\?")
        path (if (= u "/") u (strip-slashes :left u))
        query (remove-empty-matches (decode-query-params q))
        matched-path (match-path expanded-routes path)]
    (if matched-path
      (assoc matched-path :data (merge query (:data matched-path)))
      (if (and (not matched-path) (str/ends-with? path "/"))
        (let [path' (if (= u "/") u (strip-slashes :right u))
              matched-path' (match-path expanded-routes path')]
          (if matched-path'
            (assoc matched-path' :data (merge query (:data matched-path')))
            {:data query}))
        {:data query}))))

(defn map->url
  "Accepts `expanded-routes` vector (returned by the `expand-routes` function)
  and a map as arguments. Returns a URL part which is the closest representatation
  of the data contained in the map (based on the `expanded-routes` argument).

  ```clojure
  ;; define routes
  (def routes [[\":page\", {:page \"index\"}]
                \":page/:id\"
                \":page/:id/:action\"])

  (def expanded-routes (expand-routes routes))

  (map->url expanded-routes {:page \"foo\"})
  ;; \"foo\"

  (map->url expanded-routes {:page \"foo\" :id 1})
  ;; \"foo/1\"

  (map->url expanded-routes {:page \"foo\" :id 1 :action \"bar\" :qux \"baz\"})
  ;; \"foo/1/bar?qux=baz\"
  ```
  "
  [expanded-routes data]
  (if-let [best-route (get-best-route expanded-routes data)]
    (build-url best-route data)
    (str "?" (encode-query-params data))))

(defn expand-routes
  "Accepts a vector of routes as the argument. Returnes the expanded version
  of routes that can be passed to `url->map` and `map->url` functions.

  Elements in the route vector must be string (pattern) or vectors that contain
  the string pattern and default values for that route.

  ```clojure
  (def route \":page\")
  ;; This route will not be matched by an empty string

  (def route-with-defaults [\":page\", {:page \"index\"}])
  ;; This route will match an empty string and the :page key will hold
  ;; the value \"index\"

  (expand-routes [[\":page\" {:page \"index\"}]
                  \":page/:action\"])
  ;; \"\" will be matched as {:page \"index\"}
  ;; \"foo/bar\" will be matched as {:page \"foo\" :action \"bar\"}
  ```
  "
  [routes]
  (let [expanded-routes (group-by :type (map expand-route routes))]
    ;; We put routes without placeholders at the start of the list, so they would
    ;; be matched first - exact matches have precedence over matches with placeholders
    ;;
    ;; Routes that have placeholders are sorted so that the routes with the most
    ;; placeholders come first, because these have more specificity
    ;;
    ;; At the end of the list we put the catch-all route (if any exist)
    (vec (concat (expanded-routes ::exact)
                 (sort-by #(- (:specificity *)) (expanded-routes ::pattern))
                 (expanded-routes ::catch-all)))))