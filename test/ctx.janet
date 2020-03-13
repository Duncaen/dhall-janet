(use ../ctx)

(def ctx (make-ctx))
(put-ctx ctx "foo" 1)
(put-ctx ctx "foo" 2)
(assert (deep= (ctx "foo") @[2 1]))

(def newctx (make-ctx ctx))
(assert (deep= (ctx "foo") @[2 1]))
(put-ctx newctx "foo" 3)
(assert (deep= (newctx "foo") @[3 2 1]))

(assert (deep= (ctx "foo") @[2 1]))
