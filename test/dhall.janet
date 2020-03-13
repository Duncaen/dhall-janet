(import ../dhall)

(defn- dbg
  [x]
  (pp x)
  x)

(defn- assert=
  [x y]
  (assert (deep= x y)))

(-> `1 + 1`
    dhall/parse
    dhall/eval
    dhall/emit
    eval
    (assert= 2))

(-> `let x = 1 in x`
    dhall/parse
    dhall/eval
    dhall/emit
    eval
    (assert= 1))

(-> `
    let x = 2
    let mul = \(x : Natural) -> x * x
    in (mul (x + 1))
    `
    dhall/parse
    dhall/eval
    dhall/emit
    eval
    (assert= (* 3 3)))
