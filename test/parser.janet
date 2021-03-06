(import ../parser)

(defmacro- assert=
  [a b]
  ~(assert (= ,a ,b)))

(defmacro- assert!=
  [a b]
  ~(assert (not= ,a ,b)))

(assert= nil (parser/parse `{- foo -}`))
(assert!= nil (parser/parse `∀(x : a) → b`))
(assert!= nil (parser/parse `\( x : Bool ) -> x@1`))
(assert!= nil (parser/parse `\( a : y ) -> x`))
(assert!= nil (parser/parse `if a then b else c`))
(assert!= nil (parser/parse `if True then True else False`))
(assert!= nil (parser/parse `let x = True in x`))
(assert!= nil (parser/parse `let x : Annot = True in x`))
(assert!= nil (parser/parse `forall (x : a) -> b`))
(assert!= nil (parser/parse `"ABC"`))
(assert!= nil (parser/parse `"foo${fizz}bar"`))
(assert!= nil (parser/parse `"A${x}B${x}C"`))
(assert!= nil (parser/parse `"a\$\b\f\n\r\t\"\\a"`))
(assert!= nil (parser/parse `1`))
(assert!= nil (parser/parse `0x01`))
(assert!= nil (parser/parse `0xDEADBEEF`))
(assert!= nil (parser/parse `{- foo -}1`))
(assert!= nil (parser/parse `1{- foo -}`))
(assert!= nil (parser/parse `1{- foo -}1`))
(assert!= nil (parser/parse `{-foo-}1`))
(assert!= nil (parser/parse `{-
                    foo
                    -}1`))
(assert!= nil (parser/parse `{=}`))
(assert!= nil (parser/parse `{}`))
(assert!= nil (parser/parse `<>`))
(assert!= nil (parser/parse `<|>`))
(assert!= nil (parser/parse `foo ? bar`))
(assert!= nil (parser/parse `foo ? bar ? fizz ? buzz`))
(assert!= nil (parser/parse `False || True`))
(assert!= nil (parser/parse `False || False || True`))
(assert!= nil (parser/parse `1 + 1`))
(assert!= nil (parser/parse `1 + 2 + 3`))
(assert!= nil (parser/parse `"A" ++ "B"`))
(assert!= nil (parser/parse `"A" ++ "B" ++ "C"`))
(assert!= nil (parser/parse `[1] # [2]`))
(assert!= nil (parser/parse `[1] # [2] # [3]`))
(assert!= nil (parser/parse `True && False`))
(assert!= nil (parser/parse `True && True && True`))
(assert!= nil (parser/parse `{=} /\ {=}`))
(assert!= nil (parser/parse `{=} /\ {=} /\ {=}`))
(assert!= nil (parser/parse `{=} // {=}`))
(assert!= nil (parser/parse `{=} // {=} // {=}`))
(assert!= nil (parser/parse `{} //\\ {}`))
(assert!= nil (parser/parse `{} //\\ {} //\\ {}`))
(assert!= nil (parser/parse `2 * 2`))
(assert!= nil (parser/parse `2 * 2 * 2`))
(assert!= nil (parser/parse `1 == 1`))
(assert!= nil (parser/parse `1 != 1`))
(assert!= nil (parser/parse `{=} === {=}`))
(assert!= nil (parser/parse `(foo bar)`))
(assert!= nil (parser/parse ```
                            ''
                            foo bar
                            ''
                            ```))
(assert=
  ((parser/parse `TypeSynonym`) :name)
  "TypeSynonym")
