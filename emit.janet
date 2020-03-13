(defn emit
  "Convert a dhall value or term into a janet form."
  [x]
  (match x
    {:value :NaturalLiteral :n n} n
    (error (string/format "unhandled: %q" x))))
