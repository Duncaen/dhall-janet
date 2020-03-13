(defn put-ctx
  "Puts a value in to the context."
  [ctx k v]
  (if-let [vals (ctx k)]
    (put ctx k @[v ;vals])
    (put ctx k @[v])))

(defn make-ctx
  "Creates a new context table."
  [&opt parent]
  (table/setproto @{} (or parent @{})))
