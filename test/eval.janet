(import ../eval)

(defmacro- check=
  [a b]
  ~(if (not (deep= ,a ,b))
    (error (string/format "%q != %q" ,a ,b))))

(defmacro- match=
  [a b]
  ~(match ,a ,b true (error (string/format "%q != %q" ,a ,b))))

(check= (eval/value? {:value :Foo}) true)
(check= (eval/value? {:term :Foo}) false)
(check= (eval/value? {:value :Foo} :Foo) true)
(check= (eval/value? {:value :Foo} :Bar) false)
(check= (eval/value? {:term :Foo} :Foo) false)

(check=
  (eval/eval {:term :Plus
              :l {:term :NaturalLiteral :n 1}
              :r {:term :NaturalLiteral :n 1}})
  {:value :NaturalLiteral :n 2})

(check=
  (eval/eval {:term :Times
              :l {:term :NaturalLiteral :n 2}
              :r {:term :NaturalLiteral :n 2}})
  {:value :NaturalLiteral :n 4})

(check=
  (eval/eval {:term :Times
              :l {:term :NaturalLiteral :n 1}
              :r {:term :Var :name "x" :index 0}})
  {:term :Var :name "x" :index 0})

(match=
  (eval/eval {:term :Lambda
              :label "x"
              :type {:term :Natural}
              :body {:term :Var :name "x" :index 0}})
  {:label "x" :type {:term :Natural}})

(check=
  (eval/eval
    {:term :Let
     :bindings
     [{:name "x" :value {:term :NaturalLiteral :n 1}}
      {:name "y" :value {:term :Lambda
                         :label "x"
                         :type {:term :Natural}
                         :body
                         {:term :Plus
                          :l {:term :Var :name "x" :index 0}
                          :r {:term :NaturalLiteral :n 1}}
                         }}]
     :body {:term :App
            :fn {:term :Var :name "y" :index 0}
            :arg {:term :Var :name "x" :index 0}}})
  {:value :NaturalLiteral :n 2})
