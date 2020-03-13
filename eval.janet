(use ctx)

(defn value?
  "Check if x is a value and the optional type matches."
  [x &opt type]
  (if-let [t (x :value)]
    (if (nil? type) true (= t type))
    false))

(defn- app
  ""
  [call & args]
  (let [f (call :fn)]
    (var out call)
    (each arg args
      (set out (or (when (function? f) (apply f [arg]))
                   {:term :App :fn out :arg arg})))
    out))

(defn- eval-with
  [term ctx alpha]
  (match
    term
    {:term :Var :index index :name name}
    (let [len (length (or (ctx name) []))]
      (if (>= index len)
        {:term :Var :name name :index (- index len)}
        ((ctx name) index)))
    {:term :Lambda :label label :type type :body body}
    {:term :Lambda
     :label (cond alpha "_" label)
     :type (eval-with type ctx alpha)
     :fn (fn
           [x]
           (let [newctx (make-ctx ctx)]
             (put-ctx newctx label x)
             (eval-with body newctx alpha)
             ))}
    {:term :Pi :label label :type type :body body}
    {:term :Pi
     :label (cond alpha "_" label)
     :type (eval-with type ctx alpha)
     :body (fn
             [x]
             (let [newctx (make-ctx ctx)]
               (put-ctx newctx label x)
               (eval-with body newctx alpha)
               ))}
    {:term :App :fn fn :arg arg}
    (app
      (eval-with fn ctx alpha)
      (eval-with arg ctx alpha))
    {:term :Let :bindings binds :body body}
    (let [newctx (make-ctx ctx)]
      (each b binds
        (put-ctx newctx (b :name) (eval-with (b :value) newctx alpha)))
      (eval-with body newctx alpha))
    {:term :Plus :l l :r r}
    (let [l   (eval-with l ctx alpha)
          r   (eval-with r ctx alpha)
          lok (value? l :NaturalLiteral)
          rok (value? r :NaturalLiteral)]
      (if (and lok rok)
        {:value :NaturalLiteral :n (+ (l :n) (r :n))}
        (cond
          (and lok (= (l :n) 0)) r
          (and rok (= (r :n) 0)) l
          term)))
    {:term :Times :l l :r r}
    (let [l   (eval-with l ctx alpha)
          r   (eval-with r ctx alpha)
          lok (value? l :NaturalLiteral)
          rok (value? r :NaturalLiteral)]
      (if (and lok rok)
        {:value :NaturalLiteral :n (* (l :n) (r :n))}
        (cond
          (and lok (= (l :n) 0)) (break l)
          (and rok (= (r :n) 0)) (break r)
          (and lok (= (l :n) 1)) (break r)
          (and rok (= (r :n) 1)) (break l)
          term)))
    {:term :NaturalLiteral :n n} {:value :NaturalLiteral :n n}
    {:term :Natural} term
    (error (string/format "unhandled term: %q" term))
    ))

(defn eval
  "Normalize a Term to a Value"
  [term]
  (eval-with term (make-ctx) false))

(defn alpha-beta-eval
  "Alpha-Beta-Normalize a Term to a Value"
  [term]
  (eval-with term (make-ctx) true))
