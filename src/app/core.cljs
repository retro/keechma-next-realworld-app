(ns app.core
  (:require ))

(defn ^:dev/after-load render
  "Render the toplevel component for this app."
  []
  (println "RENDERING"))

(defn ^:export main
  "Run application startup logic."
  []
  (render))
