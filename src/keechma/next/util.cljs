(ns keechma.next.util)

(defn dissoc-in
  "Dissociate a value in a nested associative structure, identified by a sequence
  of keys. Any collections left empty by the operation will be dissociated from
  their containing structures."
  ([m ks]
   (if-let [[k & ks] (seq ks)]
     (if (seq ks)
       (let [v (dissoc-in (get m k) ks)]
         (if (empty? v)
           (dissoc m k)
           (assoc m k v)))
       (dissoc m k))
     m))
  ([m ks & kss]
   (if-let [[ks' & kss] (seq kss)]
     (recur (dissoc-in m ks) ks' kss)
     (dissoc-in m ks))))

(defn shallow-identical? [m1 m2]
  (if (not (and (map? m1) (map? m2)))
    false
    (reduce-kv
      (fn [_ k v]
        (if (identical? v (get m2 k))
          true
          (reduced false)))
      true
      m1)))