(ns router.core
  (:require [clojure.walk :refer [postwalk]]
            [clojure.set :refer [superset? union intersection]]
            [router.util :refer [decode-query-params encode-query-params]]
            [clojure.string :as str]))

(defn ^:private placeholder->key [p]
  (-> (subs p 1)
      keyword))

(defn ^:private process-route-part [default-keys part]
  (case part
    "/" {:part part :re-match (str "/")}
    "." {:part part :re-match (str "\\.")}
    (let [is-placeholder (= ":" (first part))
          is-splat (and is-placeholder (= "*" (last part)))
          key (when is-placeholder (placeholder->key part))
          has-default (contains? default-keys key)
          re-match (cond
                     is-splat "(.*)"
                     is-placeholder (str "(" "[^/]" (if has-default "*" "+") ")")
                     :else part)]
      {:is-placeholder is-placeholder
       :is-splat is-splat
       :is-static (not is-placeholder)
       :key key
       :part part
       :has-default has-default
       :re-match re-match})))

(defn ^:private route-regex [parts]
  (let [base-regex (str/join (map (fn [p] (:re-match p)) parts))
        full-regex (str "^/?" base-regex "/?$")]
    (re-pattern full-regex)))

(defn ^:private route-placeholders [parts]
  (->> parts
       (map :key)
       (remove nil?)))

(defn route-splats [parts]
  (->> parts
       (filter #(:is-splat %))
       route-placeholders))

(defn route-statics [parts]
  (->> parts
       (filter #(:is-static %))))

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
          (throw (ex-info "Mismatched braces" {:route r}))

          (= "*" (last part))
          (throw (ex-info "Splat must be the last character in the placeholder" {:route r :placeholder part}))

          :else
          (recur rest-route (conj part c)))))))

(defn consume-brace-placeholder [r]
  (loop [route r
         part []]
    (if-not (seq route)
      (throw (ex-info "Missing closing brace" {:route r}))
      (let [[c & rest-route] route]
        (cond
          (and (= ":" c) (nil? (seq part)))
          (recur rest-route part)

          (= ":" c)
          (throw (ex-info "Colon must be the first character in the placeholder" {:route r :placeholder part}))

          (= "}" c)
          {:part (str/join part) :rest-route rest-route}

          (= "*" (last part))
          (throw (ex-info "Splat must be the last character in the placeholder" {:route r :placeholder part}))

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

(defn throw-if-duplicate-placeholders [route placeholders]
  (let [placeholder-freqs (frequencies placeholders)
        duplicates (map first (filter (fn [[k v]] (< 1 v)) placeholder-freqs))]
    (when (seq duplicates)
      (throw (ex-info "Duplicate placeholders" {:route route :duplicates duplicates})))))

(defn ^:private process-route [[route defaults]]
  (let [parts (route->parts route)
        processed-parts (map (partial process-route-part (set (keys defaults))) parts)
        all-placeholders (route-placeholders processed-parts)
        placeholders (set all-placeholders)
        splats (set (route-splats processed-parts))
        statics (route-statics processed-parts)
        placeholder-count (count placeholders)
        splat-count (count splats)]
    (throw-if-duplicate-placeholders route all-placeholders)
    {:parts processed-parts
     :regex (route-regex processed-parts)
     :placeholders placeholders
     :splats splats
     :route route
     :defaults (or defaults {})
     :matchable-keys (union placeholders (set (keys defaults)))
     :type (cond
             (seq splats) ::splat
             (seq placeholders) ::pattern
             :else ::exact)
     :specificity (+ (- placeholder-count splat-count)
                     (* 10 (count statics))
                     (* 1.001 (count defaults))
                     (* 0.01 splat-count))}))

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
    (fn [_ route]
      (when-let [matches (match-path-with-route route path)]
        (reduced {:route (:route route)
                  :data (merge (:defaults route) (remove-empty-matches matches))})))
    nil
    expanded-routes))

(defn ^:private get-url-segment [state k is-splat]
  (let [defaults (:defaults state)
        data (:data state)
        default (get defaults k)
        val (get data k)
        encode (if is-splat js/encodeURI js/encodeURIComponent)]
    (if (= default val) "" (encode val))))

(defn dissoc-defaults [data defaults]
  (reduce-kv
    (fn [acc k v]
      (if-not (seq acc)
        (reduced nil)
        (if (= (get acc k) v)
          (dissoc acc k)
          acc)))
    data
    defaults))

(defn build-url-parts [data {:keys [defaults parts]}]
  (let [{:keys [data defaults url]}
        (reduce
          (fn [acc part]
            (if-let [key (:key part)]
              {:data (dissoc (:data acc) key)
               :defaults (dissoc (:defaults acc) key)
               :url (conj (:url acc) (get-url-segment acc key (:is-splat part)))}
              (assoc acc :url (conj (:url acc) (:part part)))))
          {:data data :defaults defaults :url []}
          parts)]
    {:url (str/join url)
     :query-params (dissoc-defaults data defaults)}))

(defn build-url [data route]
  (let [{:keys [query-params url]} (build-url-parts data route)]
    (if-not (seq query-params)
      (if (= "/" url) "" url)
      (str url "?" (encode-query-params query-params)))))

(defn ^:private get-route-score [data {:keys [placeholders defaults matchable-keys]}]
  (reduce
    (fn [score k]
      (let [datum (get data k)
            default (get defaults k)
            matches-placeholder (and datum (contains? placeholders k))
            matches-default (and datum default (= datum default))]
        (cond
          matches-placeholder (inc score)
          matches-default (+ score 1.001)
          :else (* 0.9 score))))
    0
    matchable-keys))

(defn get-best-route [expanded-routes data]
  (-> (reduce
        (fn [[best-route best-score] route]
          (let [score (get-route-score data route)]
            (if (< best-score score)
              [route score]
              [best-route best-score])))
        [nil 0]
        expanded-routes)
      first))

(defn sort-by-specificity [routes]
  (sort-by #(- (:specificity %)) routes))

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
  (let [[path q] (str/split url #"\?")
        query (remove-empty-matches (decode-query-params q))
        matched-path (match-path expanded-routes path)]
    (if matched-path
      (assoc matched-path :data (merge query (:data matched-path)))
      {:data query})))

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
    (build-url data best-route)
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
    ;; At the end of the list we put the routes with splats
    (vec (concat (expanded-routes ::exact)
                 (sort-by-specificity (expanded-routes ::pattern))
                 (sort-by-specificity (expanded-routes ::splat))))))