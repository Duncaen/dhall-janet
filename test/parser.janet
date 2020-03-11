(import ../parser)

(pp (parser/parse `\( x : Bool ) -> x@1`))
(pp (parser/parse `\( a : y ) -> x`))
(pp (parser/parse `if a then b else c`))
(pp (parser/parse `if True then True else False`))
(pp (parser/parse `let x = True in x`))
(pp (parser/parse `let x : Annot = True in x`))
(pp (parser/parse `forall (x : a) -> b`))
(pp (parser/parse `∀(x : a) → b`))
(pp (parser/parse `"ABC"`))
(pp (parser/parse `"A" ++ "B"`))
(pp (parser/parse `"foo${fizz}bar"`))
(pp (parser/parse `"A${x}B${x}C"`))
