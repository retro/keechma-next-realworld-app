(ns keechma.next.controllers.entitydb.protocols)

(defprotocol IEntityDbApi
  (insert-entity! [_ entity-type data])
  (insert-many! [_ entity-type entities])
  (insert-named! [_ entity-type entity-name data] [_ entity-type entity-name data n-meta])
  (insert-collection! [_ entity-type collection-name data] [_ entity-type collection-name data c-meta])
  (remove-entity! [_ entity-type id])
  (remove-named! [_ entity-name])
  (remove-collection! [_ collection-name]))