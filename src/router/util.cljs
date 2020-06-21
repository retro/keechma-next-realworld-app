(ns router.util
  (:require [clojure.string :as string]
            [clojure.walk :refer [keywordize-keys]]))

;; this is taken from the Secretary project and slightly modified
;; https://github.com/clj-commons/secretary/blob/master/src/secretary/core.cljs

;;----------------------------------------------------------------------
;; Parameter encoding

(def encode js/encodeURIComponent)

(defmulti
  ^{:private true
    :doc "Given a key and a value return and encoded key-value pair."}
  encode-pair
  (fn [[k v]]
    (cond
      (or (sequential? v) (set? v))
      ::sequential
      (or (map? v) (satisfies? IRecord v))
      ::map)))

(defn- key-index
  ([k] (str (name k) "[]"))
  ([k index]
   (str (name k) "[" index "]")))

(defn name-with-ns [k]
  (if (keyword? k)
    (let [k-namespace (namespace k)
          k-name (name k)]
      (if k-namespace
        (str k-namespace "/" k-name)
        k-name))
    k))

(defmethod encode-pair ::sequential [[k v]]
  (let [encoded (map-indexed
                  (fn [i x]
                    (let [pair (if (coll? x)
                                 [(key-index k i) x]
                                 [(key-index k) x])]
                      (encode-pair pair)))
                  v)]
    (string/join \& encoded)))

(defmethod encode-pair ::map [[k v]]
  (let [encoded (map
                  (fn [[ik iv]]
                    (encode-pair [(key-index (name-with-ns k) (name-with-ns ik)) iv]))
                  v)]
    (string/join \& encoded)))

(defmethod encode-pair :default [[k v]]
  (str (name-with-ns k) \= (encode (str v))))

(defn encode-query-params
  "Convert a map of query parameters into url encoded string."
  [query-params]
  (string/join \& (map encode-pair query-params)))

(defn encode-uri
  "Like js/encodeURIComponent excepts ignore slashes."
  [uri]
  (->> (string/split uri #"/")
       (map encode)
       (string/join "/")))

;;----------------------------------------------------------------------
;; Parameter decoding

(def decode js/decodeURIComponent)

(defn- parse-path
  "Parse a value from a serialized query-string key index. If the
  index value is empty 0 is returned, if it's a digit it returns the
  js/parseInt value, otherwise it returns the extracted index."
  [path]
  (let [index-re #"\[([^\]]*)\]*" ;; Capture the index value.
        parts (re-seq index-re path)]
    (map
      (fn [[_ part]]
        (cond
          (empty? part) 0
          (re-matches #"\d+" part) (js/parseInt part)
          :else part))
      parts)))

(defn- key-parse
  "Return a key path for a serialized query-string entry.
  Ex.
    (key-parse \"foo[][a][][b]\")
    ;; => (\"foo\" 0 \"a\" 0 \"b\")
  "
  [k]
  (let [re #"([^\[\]]+)((?:\[[^\]]*\])*)?"
        [_ key path] (re-matches re k)
        parsed-path (when path (parse-path path))]
    (cons key parsed-path)))

(defn- assoc-in-query-params
  "Like assoc-in but numbers in path create vectors instead of maps.
  Ex.
    (assoc-in-query-params {} [\"foo\" 0] 1)
    ;; => {\"foo\" [1]}
    (assoc-in-query-params {} [\"foo\" 0 \"a\"] 1)
    ;; => {\"foo\" [{\"a\" 1}]}
  "
  [m path v]
  (let [heads (fn [xs]
                (map-indexed
                  (fn [i _]
                    (take (inc i) xs))
                  xs))
        hs (heads path)
        m (reduce
            (fn [m h]
              (if (and (or (number? (last h)))
                       (not (vector? (get-in m (butlast h)))))
                (assoc-in m (butlast h) [])
                m))
            m
            hs)]
    (if (zero? (last path))
      (update-in m (butlast path) conj v)
      (assoc-in m path v))))

(defn decode-query-params
  "Extract a map of query parameters from a query string."
  [query-string]
  (let [parts (string/split query-string #"&")
        params (reduce
                 (fn [m part]
                   ;; We only want two parts since the part on the right hand side
                   ;; could potentially contain an =.
                   (let [[k v] (string/split part #"=" 2)]
                     (assoc-in-query-params m (key-parse (decode k)) (decode v))))
                 {}
                 parts)
        params (keywordize-keys params)]
    params))