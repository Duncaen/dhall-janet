(def- pat
  (peg/compile
    ~{
      :end-of-line (+ "\n" "\r\n")

      # This rule matches all characters that are not:
      #
      # * not ASCII
      # * not part of a surrogate pair
      # * not a "non-character"
      :valid-non-ascii 0

      :tab "\t"

      :block-comment (* "{-" :block-comment-continue)
      :block-comment-char (* (range "\x20\x7F" :tab :end-of-line))
      :block-comment-continue (+ "-}" (* :block-comment))

      :not-end-of-line (+ :w :tab (set " "))

      # NOTE: Slightly different from Haskell-style single-line comments because this
      # does not require a space after the dashes
      :line-comment (* "--" :end-of-line)

      :whitespace-chunk (+ " " :tab :end-of-line :line-comment :block-comment)
      :whsp (any :whitespace-chunk)

      # nonempty whitespace
      :whsp1 (some :whitespace-chunk)

      # Uppercase or lowercase ASCII letter
      :ALPHA (range "az" "AZ")

      # ASCII digit
      :DIGIT (range "09")

      :ALPHANUM (range "09" "az" "AZ")

      :HEXDIG (+ :DIGIT (range "AF"))

      # A simple label cannot be one of the reserved keywords
      # listed in the `keyword` rule.
      # A PEG parser could use negative lookahead to
      # enforce this, e.g. as follows:
      # simple-label =
      #       keyword 1*simple-label-next-char
      #     / !keyword (simple-label-first-char *simple-label-next-char)
      :simple-label-first-char (+ :ALPHA "_")
      :simple-label-next-char (+ :ALPHANUM "-" "/" "_")
      #:simple-label (* :simple-label-first-char (any :simple-label-next-char))
      :simple-label
        (+
          (* :keyword (some :simple-label-next-char))
          (* (! :keyword) (* :simple-label-first-char (any :simple-label-next-char))))

      :quoted-label-char (+ (range "\x20\x5F") (range "\x61\x7E"))
      :quoted-label (some :quoted-label-char)

      # NOTE: Dhall does not support Unicode labels, mainly to minimize the potential
      # for code obfuscation
      :label (+ (* "`" :quoted-label "`") :simple-label)

      # A nonreserved-label cannot not be any of the reserved identifiers for builtins
      # (unless quoted).
      # Their list can be found in the `builtin` rule.
      # The only place where this restriction applies is bound variables.
      # A PEG parser could use negative lookahead to avoid parsing those identifiers,
      # e.g. as follows:
      # nonreserved-label =
      #      builtin 1*simple-label-next-char
      #    / !builtin label
      :nonreserved-label
        (+ (* :builtin (some :simple-label-next-char))
           (* (! :builtin) :label))

      # An any-label is allowed to be one of the reserved identifiers (but not a keyword).
      :any-label :label

      # Allow specifically `Some` in record and union labels.
      :any-label-or-some (+ :any-label :Some)


      # Dhall's double-quoted strings are similar to JSON strings (RFC7159) except:
      #
      # * Dhall strings support string interpolation
      #
      # * Dhall strings also support escaping string interpolation by adding a new
      #   `\$` escape sequence
      #
      # * Dhall strings also allow Unicode escape sequences of the form `\u{XXX}`
      :double-quote-chunk
        (+
         :interpolation
         # '\'    Beginning of escape sequence
         (* "\x5C" :double-quote-escaped)
         (<- :double-quote-char))

      :double-quote-escaped
        (+
          "\x22"                      # '"'    quotation mark  U+0022
          "\x24"                      # '$'    dollar sign     U+0024
          "\x5C"                      # '\'    reverse solidus U+005C
          "\x2F"                      # '/'    solidus         U+002F
          "\x62"                      # 'b'    backspace       U+0008
          "\x66"                      # 'f'    form feed       U+000C
          "\x6E"                      # 'n'    line feed       U+000A
          "\x72"                      # 'r'    carriage return U+000D
          "\x74"                      # 't'    tab             U+0009
          (* "\x75" :unicode-escape)) # 'uXXXX' / 'u{XXXX}'    U+XXXX

      # Valid Unicode escape sequences are as follows:
      #
      # * Exactly 4 hexadecimal digits without braces:
      #       `\uXXXX`
      # * 1-6 hexadecimal digits within braces (with optional zero padding):
      #       `\u{XXXX}`, `\u{000X}`, `\u{XXXXX}`, `\u{00000XXXXX}`, etc.
      #   Any number of leading zeros are allowed within the braces preceding the 1-6
      #   digits specifying the codepoint.
      #
      # From these sequences, the parser must also reject any codepoints that are in
      # the following ranges:
      #
      # * Surrogate pairs: `%xD800-DFFF`
      # * Non-characters: `%xNFFFE-NFFFF` / `%x10FFFE-10FFFF` for `N` in `{ 0 .. F }`
      #
      # See the `valid-non-ascii` rule for the exact ranges that are not allowed
      :unicode-escape (+ :unbraced-escape (* "{" :braced-escape "}"))

      # All valid last 4 digits for unicode codepoints (outside Plane 0): `0000-FFFD`
      :unicode-suffix 
        (+
         (* (+ :DIGIT "A" "B" "C" "D" "E") :HEXDIG :HEXDIG :HEXDIG)
         (* "F" :HEXDIG :HEXDIG (+ :DIGIT "A" "B" "C" "D")))

      # All 4-hex digit unicode escape sequences that are not:
      #
      # * Surrogate pairs (i.e. `%xD800-DFFF`)
      # * Non-characters (i.e. `%xFFFE-FFFF`)
      #
      :unbraced-escape
        (+
          (* (+ :DIGIT "A" "B" "C") :HEXDIG :HEXDIG :HEXDIG)
          (* "D" (+ "0" "1" "2" "3" "4" "5" "6" "7") :HEXDIG :HEXDIG)
          # %xD800-DFFF Surrogate pairs
          (* "E" :HEXDIG :HEXDIG :HEXDIG)
          (* "F" :HEXDIG :HEXDIG (+ :DIGIT "A" "B" "C" "D")))
          # %xFFFE-FFFF Non-characters

      # All 1-6 digit unicode codepoints that are not:
      #
      # * Surrogate pairs: `%xD800-DFFF`
      # * Non-characters: `%xNFFFE-NFFFF` / `%x10FFFE-10FFFF` for `N` in `{ 0 .. F }`
      #
      # See the `valid-non-ascii` rule for the exact ranges that are not allowed
      :braced-codepoint
        (+
         (* (+ "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" "10") :unicode-suffix) # (Planes 1-16)
         :unbraced-escape # (Plane 0)
         (* :HEXDIG :HEXDIG :HEXDIG)) # %x000-FFF


      # Allow zero padding for braced codepoints
      :braced-escape (* (any "0") :braced-codepoint)

      # Printable characters except double quote and backslash
      :double-quote-char
        (+
         (range "\x20\x21")
         # %x22 = '"'
         (range "\x23\x5B")
         # %x5C = "\"
         (range "\x5D\x7F")
         :valid-non-ascii)

      :double-quote-literal
        (cmt (* "\x22" (any :double-quote-chunk) "\x22")
             ,(fn double-quote-literal
                [& xs]
                (var str @"")
                (var chunks @[])
                (each x xs
                  (case (type x)
                    :string (buffer/push-string str x)
                    :struct (do
                      (array/push chunks
                        {:type :Chunk :Prefix (string str) :E x})
                      (buffer/clear str))
                    (error "unhandled type in double-quoted-literal")))
                {:type :TextLit :Chunks chunks :Suffix (string str)}))

      # NOTE: The only way to end a single-quote string literal with a single quote is
      # to either interpolate the single quote, like this:
      #
      #     ''ABC${"'"}''
      #
      # ... or concatenate another string, like this:
      #
      #     ''ABC'' ++ "'"
      #
      # If you try to end the string literal with a single quote then you get "'''",
      # which is interpreted as an escaped pair of single quotes
      :single-quote-continue
        (+
          (* :interpolation         :single-quote-continue)
          (* :escaped-quote-pair    :single-quote-continue)
          (* :escaped-interpolation :single-quote-continue)
          "''" # End of text literal
          (* :single-quote-char     :single-quote-continue))

      # Escape two single quotes (i.e. replace this sequence with "''")
      :escaped-quote-pair "'''"

      # Escape interpolation (i.e. replace this sequence with "${")
      :escaped-interpolation "''${"

      :single-quote-char
        (+ (range "\x20\x7F")
           :valid-non-ascii
           :tab
           :end-of-line)

      :single-quote-literal (* "''" :end-of-line :single-quote-continue)

      :interpolation (* "${" :complete-expression "}")

      :text-literal (+ :double-quote-literal :single-quote-literal)

      :if "if"
      :then "then"
      :else "else"
      :let "let"
      :in "in"
      :as "as"
      :using "using"
      :merge "merge"
      :missing "missing"
      :Infinity "Infinity"
      :NaN "NaN"
      :Some "Some"
      :toMap "toMap"
      :assert "assert"
      :forall (+ "∀" "forall")
      :with "with"

      # Unused rule that could be used as negative lookahead in the
      # `simple-label` rule for parsers that support this.
      :keyword (+ :if :then :else
                  :let :in
                  :using :missing
                  :assert :as
                  :Infinity :NaN
                  :merge :Some :toMap
                  :forall
                  :with)

      :builtin
       (+
        (* :Natural-fold      (constant {:type :Natural-fold}))
        (* :Natural-build     (constant {:type :Natural-build}))
        (* :Natural-isZero    (constant {:type :Natural-isZero}))
        (* :Natural-even      (constant {:type :Natural-even}))
        (* :Natural-odd       (constant {:type :Natural-odd}))
        (* :Natural-toInteger (constant {:type :Natural-toInteger}))
        (* :Natural-show      (constant {:type :Natural-show}))
        (* :Integer-toDouble  (constant {:type :Integer-toDouble}))
        (* :Integer-show      (constant {:type :Integer-show}))
        (* :Integer-negate    (constant {:type :Integer-negate}))
        (* :Integer-clamp     (constant {:type :Integer-clamp}))
        (* :Natural-subtract  (constant {:type :Natural-substract}))
        (* :Double-show       (constant {:type :Double-show}))
        (* :List-build        (constant {:type :List-build}))
        (* :List-fold         (constant {:type :List-fold}))
        (* :List-length       (constant {:type :List-length}))
        (* :List-head         (constant {:type :List-head}))
        (* :List-last         (constant {:type :List-last}))
        (* :List-indexed      (constant {:type :List-indexed}))
        (* :List-reverse      (constant {:type :List-reverse}))
        (* :Optional-fold     (constant {:type :Optional-fold}))
        (* :Optional-build    (constant {:type :Optional-build}))
        (* :Text-show         (constant {:type :Text-show}))
        (* :Bool              (constant {:type :Bool}))
        (* :True              (constant {:type :True}))
        (* :False             (constant {:type :False}))
        (* :Optional          (constant {:type :Optional}))
        (* :None              (constant {:type :None}))
        (* :Natural           (constant {:type :Natural}))
        (* :Integer           (constant {:type :Integer}))
        (* :Double            (constant {:type :Double}))
        (* :Text              (constant {:type :Text}))
        (* :List              (constant {:type :List}))
        (* :Type              (constant {:type :Type}))
        (* :Kind              (constant {:type :Kind}))
        (* :Sort              (constant {:type :Sort})))

      # Reserved identifiers, needed for some special cases of parsing
      :Optional "Optional"
      :Text     "Text"
      :List     "List"
      :Location "Location"

      # Reminder of the reserved identifiers, needed for the `builtin` rule
      :Bool              "Bool"
      :True              "True"
      :False             "False"
      :None              "None"
      :Natural           "Natural"
      :Integer           "Integer"
      :Double            "Double"
      :Type              "Type"
      :Kind              "Kind"
      :Sort              "Sort"
      :Natural-fold      "Natural/fold"
      :Natural-build     "Natural/build"
      :Natural-isZero    "Natural/isZero"
      :Natural-even      "Natural/even"
      :Natural-odd       "Natural/odd"
      :Natural-toInteger "Natural/toInteger"
      :Natural-show      "Natural/show"
      :Natural-subtract  "Natural/substract"
      :Integer-toDouble  "Natural/toDouble"
      :Integer-show      "Integer/show"
      :Integer-negate    "Integer/negate"
      :Integer-clamp     "Integer/clamp"
      :Double-show       "Double/show"
      :List-build        "List/build"
      :List-fold         "List/fold"
      :List-length       "List/length"
      :List-head         "List/head"
      :List-last         "List/last"
      :List-indexed      "List/indexed"
      :List-reverse      "List/reverse"
      :Optional-fold     "Optional/fold"
      :Optional-build    "Optional/build"
      :Text-show         "Text/show"

      # Operators
      :combine (+ "\x2227" "/\\")
      :combine-types (+ "\x2A53" "//\\\\")
      :equivalent (+ "\x2261" "===")
      :prefer (+ "\x2AFD" "//")
      :lambda (+ "\x2BB" "\\")
      :arrow (+ "→" "->")
      :complete "::"

      :exponent (* "e" (? (+ "+" "-")) (some :DIGIT))

      :numeric-double-literal (* (? (+ "+" "-")) (some :DIGIT) (+ (* "." (some :DIGIT) (? :exponent)) :exponent))

      :minus-infinity-literal (* "-" :Infinity)
      :plus-infinity-literal :Infinity

      :double-literal
        (+
         # "2.0"
         :numeric-double-literal
         # "-Infinity"
         :minus-infinity-literal
         # "Infinity"
         :plus-infinity-literal
         # "NaN"
         :NaN)

      :natural-literal
        (+
         # Hexadecimal with "0x" prefix
         (* "0x" (some :HEXDIG))
         # Decimal; leading 0 digits are not allowed
         (cmt (<- (* (range "19") (any :DIGIT))) ,scan-number)
         # ... except for 0 itself
         (* "0" (constant 0)))

      :integer-literal (* (+ "+" "-") :natural-literal)

      # If the identifier matches one of the names in the `builtin` rule, then it is a
      # builtin, and should be treated as the corresponding item in the list of
      # "Reserved identifiers for builtins" specified in the `standard/README.md` document.
      # It is a syntax error to specify a de Bruijn index in this case.
      # Otherwise, this is a variable with name and index matching the label and index.
      :identifier (+ :variable :builtin)

      :variable (cmt (* (<- :nonreserved-label) (? (* :whsp "@" :whsp :natural-literal)))
                     ,(fn variable
                        [a &opt b]
                        (default b 0)
                        {:type :Var :l a :n b}))

      # :import (* :import-hashed (? (* :whsp :as :whsp1 (+ :Text :Location))))

      :import (* "import")

      :expression
        (+
         # "\(x : a) -> b"
         (cmt (* :lambda :whsp "(" :whsp (<- :nonreserved-label) :whsp ":" :whsp1 :expression :whsp ")"
                 :whsp :arrow :whsp :expression)
              ,(fn lambda [x y z] {:type :Lam :A x :B y :C z}))

         # "if a then b else c"
         (cmt (* :if :whsp1 :expression :whsp :then :whsp1 :expression :whsp :else :whsp1 :expression)
              ,(fn if [a b c] {:type :If :A a :B b :C c}))

         # "let x : t = e1 in e2"
         # "let x     = e1 in e2"
         # We allow dropping the `in` between adjacent let-expressions; the following are equivalent:
         # "let x = e1 let y = e2 in e3"
         # "let x = e1 in let y = e2 in e3"
         (cmt (* (some :let-binding) :in :whsp1 :expression)
              ,(fn let [a b] {:type :Let :Bindings a :A b}))

         # "forall (x : a) -> b"
         (cmt (* :forall :whsp "(" :whsp (<- :nonreserved-label) :whsp ":" :whsp1 :expression :whsp ")"
                 :whsp :arrow :whsp :expression)
              ,(fn forall [x a b] {:type :Forall :X x :A a :B b}))

         #  "a -> b"
         #
         # NOTE: Backtrack if parsing this alternative fails
         (cmt (* :operator-expression :whsp :arrow :whsp :expression)
              ,(fn operator-expression [a b] {type :OpExpr :A a :B b}))

         # "merge e1 e2 : t"
         #
         # NOTE: Backtrack if parsing this alternative fails since we can't tell
         # from the keyword whether there will be a type annotation or not
         (* :merge :whsp1 :import-expression :whsp1 :import-expression :whsp
            ":" :whsp1 :application-expression)

         # "[] : t"
         #
         # NOTE: Backtrack if parsing this alternative fails since we can't tell
         # from the opening bracket whether or not this will be an empty list or
         # a non-empty list
         :empty-list-literal

         # "toMap e : t"
         #
         # NOTE: Backtrack if parsing this alternative fails since we can't tell
         # from the keyword whether there will be a type annotation or not
         (* :toMap :whsp1 :import-expression :whsp ":" :whsp1 :application-expression)

         # "assert : Natural/even 1 === False"
         (* :assert :whsp ":" :whsp1 :expression)

         # "x : t"
         :annotated-expression)

      # Nonempty-whitespace to disambiguate `env:VARIABLE` from type annotations
      :annotated-expression (* :operator-expression (? (* :whsp ":" :whsp1 :expression)))

      # "let x = e1"
      :let-binding (cmt (* :let :whsp1 (<- :nonreserved-label) :whsp (? (* ":" :whsp1 :expression :whsp)) "="
                           :whsp :expression :whsp)
                        ,(fn binding
                           [a & xs]
                           (case (length xs)
                             2 {:type :Binding :A a :B (xs 0) :C (xs 1)}
                             1 {:type :Binding :A a :B nil    :C (xs 0)}
                             (error (string/format "got unhandled length: %d: %q" (length xs) xs)))))

      # "[] : t"
      :empty-list-literal
        (* "[" :whsp (? (* "," :whsp)) "]" :whsp ":" :whsp1 :application-expression)

      :operator-expression :import-alt-expression

      # Nonempty-whitespace to disambiguate `http://a/a?a`
      :import-alt-expression    (* :or-expression            (any (* :whsp "?" :whsp1 :or-expression)))
      :or-expression            (* :plus-expression          (any (* :whsp "||" :whsp :plus-expression)))
      # Nonempty-whitespace to disambiguate `f +2`
      :plus-expression          (* :text-append-expression   (any (* :whsp "+"  :whsp1 :text-append-expression)))
      :text-append-expression   (* :list-append-expression   (any (* :whsp "++" :whsp  :list-append-expression)))
      :list-append-expression   (* :and-expression           (any (* :whsp "#"  :whsp  :and-expression)))
      :and-expression           (* :combine-expression       (any (* :whsp "&&" :whsp  :combine-expression)))
      :combine-expression       (* :prefer-expression        (any (* :whsp :combine :whsp :prefer-expression)))
      :prefer-expression        (* :combine-types-expression (any (* :whsp :prefer  :whsp :combine-types-expression)))
      :combine-types-expression (* :times-expression         (any (* :whsp :combine-types :whsp :times-expression)))
      :times-expression         (* :equal-expression         (any (* :whsp "*"  :whsp  :equal-expression)))
      :equal-expression         (* :not-equal-expression     (any (* :whsp "==" :whsp  :not-equal-expression)))
      :not-equal-expression     (* :equivalent-expression    (any (* :whsp "!=" :whsp  :equivalent-expression)))
      :equivalent-expression    (* :with-expression          (any (* :whsp :equivalent :whsp :with-expression)))

      :with-expression (* :application-expression
                          (any (* :whsp1 :with :whsp1 :any-label-or-some
                                  (any (* :whsp "." :whsp :any-label-or-some))
                                  :whsp "=" :whsp :application-expression)))

      # Import expressions need to be separated by some whitespace, otherwise there
      # would be ambiguity: `./ab` could be interpreted as "import the file `./ab`",
      # or "apply the import `./a` to label `b`"
      :application-expression (* :first-application-expression (any (* :whsp1 :import-expression)))

      :first-application-expression
        (+
         # "merge e1 e2"
         (* :merge :whsp1 :import-expression :whsp1 :import-expression)
         # "Some e"
         (* :Some :whsp1 :import-expression)
         # "toMap e"
         (* :toMap :whsp1 :import-expression)
         :import-expression)

      :import-expression (+ :import :completion-expression)

      :completion-expression (* :selector-expression (? (* :whsp :complete :whsp :selector-expression)))

      # `record.field` extracts one field of a record
      #
      # `record.{ field0, field1, field2 }` projects out several fields of a record
      #
      # NOTE: Backtrack when parsing the `*("." ...)`.  The reason why is that you
      # can't tell from parsing just the period whether "foo." will become "foo.bar"
      # (i.e. accessing field `bar` of the record `foo`) or `foo./bar` (i.e. applying
      # the function `foo` to the relative path `./bar`)
      :selector-expression (* :primitive-expression (any (* :whsp "." :whsp :selector)))

      :selector (+ :any-label :labels :type-selector)

      :labels (* "{" :whsp (? (* :any-label-or-some :whsp (any (* "," :whsp :any-label-or-some :whsp)))) "}")

      :type-selector (* "(" :whsp :expression :whsp ")")

      # NOTE: Backtrack when parsing the first three alternatives (i.e. the numeric
      # literals).  This is because they share leading characters in common

      :primitive-expression
        (+
         # "2.0"
         :double-literal
         # "2"
         :natural-literal
         # "+2"
         :integer-literal
         # '"ABC"'
         :text-literal
         # "{ foo = 1      , bar = True }"
         # "{ foo : Integer, bar : Bool }"
         (* "{" :whsp (? (* "," :whsp)) :record-type-or-literal :whsp "}")
         # "< Foo : Integer | Bar : Bool >"
         # "< Foo | Bar : Bool >"
         (* "<" :whsp (? (* "|" :whsp)) :union-type :whsp ">")
         # "[1, 2, 3]"
         :non-empty-list-literal
         # "x"
         # "x@2"
         :identifier
         # "( e )"
         (* "(" :complete-expression ")"))

      :record-type-or-literal
        (+
         :empty-record-literal
         :non-empty-record-type-or-literal
         :empty-record-type)

      :empty-record-literal "="
      :empty-record-type ""

      :non-empty-record-type-or-literal (+ :non-empty-record-literal :non-empty-record-type)

      :non-empty-record-type (* :record-type-entry (any (* :whsp "," :whsp :record-type-entry)))

      :record-type-entry (* :any-label-or-some :whsp ":" :whsp1 :expression)

      :non-empty-record-literal (* :record-literal-entry (any (* :whsp "," :whsp :record-literal-entry)))

      :record-literal-entry (* :any-label-or-some
                               (any (* :whsp "." :whsp :any-label-or-some))
                               :whsp "=" :whsp :expression)

      :union-type (+ :non-empty-union-type :empty-union-type)

      :empty-union-type ""

      :non-empty-union-type (* :union-type-entry (any (* :whsp "|" :whsp :union-type-entry)))

      # x : Natural
      # x
      :union-type-entry (* :any-label-or-some (? (* :whsp ":" :whsp1 :expression)))

      :non-empty-list-literal
        (* "[" :whsp (? (* "," :whsp)) :expression :whsp (any (* "," :whsp :expression :whsp)) "]")

      :complete-expression (* :whsp :expression :whsp)
      :main :complete-expression
      }))

(defn parse
  ""
  [chunk]
  (peg/match pat chunk))
