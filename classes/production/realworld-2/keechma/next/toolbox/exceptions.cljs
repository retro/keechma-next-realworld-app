(ns keechma.next.toolbox.exceptions)

(defn keechma-toolbox-v2-ex-info
  ([message anomaly] (keechma-toolbox-v2-ex-info message anomaly {}))
  ([message anomaly props]
   (ex-info message (assoc props 
                           :entitydb.anomalies/category anomaly
                           :entitydb.anomalies/message message))))

(defn aborted-ex-info []
  (keechma-toolbox-v2-ex-info "Aborted" :aborted))
