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

(defn get-dirty-deps [prev-deps next-deps]
  (let [dirty
        (reduce
          (fn [m k]
            (let [v (get next-deps k)]
              (if-not (identical? v (get prev-deps k))
                (assoc m k v)
                m)))
          {}
          (set (concat (keys prev-deps) (keys next-deps))))]
    (if (empty? dirty)
      nil
      dirty)))

(defn lexicographic-compare
  ([xs ys]
   (lexicographic-compare compare xs ys))
  ([compare xs ys]
   (loop [xs (seq xs) ys (seq ys)]
     (if xs
       (if ys
         (let [c (compare (first xs) (first ys))]
           (if (not (zero? c))
             c
             (recur (next xs), (next ys))))
         1)
       (if ys
         -1
         0)))))

(defn sort-paths [paths]
  (sort lexicographic-compare paths))

(defn find-common-subvec [v1 v2]
  (let [max-idx (min (count v1) (count v2))]
    (if (pos? max-idx)
      (loop [idx 0]
        (if (and (= (get v1 idx) (get v2 idx))
                 (< idx max-idx))
          (recur (inc idx))
          (subvec v1 0 idx)))
      [])))

(defn get-lowest-common-ancestor-for-paths [paths]
  (let [[f & r] (reverse (sort paths))]
    (reduce
      (fn [acc v]
        (let [common-subvec (find-common-subvec acc v)]
          (if (= [] common-subvec)
            (reduced [])
            common-subvec)))
      f
      r)))