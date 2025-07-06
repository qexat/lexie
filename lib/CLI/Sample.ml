open Custom
open AIL

let id = Name.of_string_exn "id"
let hummingbird = Name.of_string_exn "hummingbird"
let vireo = Name.of_string_exn "vireo"
let delta = Name.of_string_exn "delta"
let _A = Name.of_string_exn "A"
let _B = Name.of_string_exn "B"
let _C = Name.of_string_exn "C"
let a = Name.of_string_exn "a"
let b = Name.of_string_exn "b"
let c = Name.of_string_exn "c"
let _T = Name.of_string_exn "T"
let _Nat = Name.of_string_exn "Nat"
let _Unit = Name.of_string_exn "Unit"
let x = Name.of_string_exn "x"
let _S = Name.of_string_exn "S"
let _O = Name.of_string_exn "O"

let lambda3 =
  fun param1 param2 param3 ret ->
  Term.lambda
    param1
    (Term.lambda param2 (Term.lambda param3 ret))
;;

let lambda6 =
  fun param1 param2 param3 param4 param5 param6 ret ->
  lambda3
    param1
    param2
    param3
    (lambda3 param4 param5 param6 ret)
;;

let arrow2 =
  fun param1 param2 ret ->
  Type.arrow
    (Named (a, param1))
    (Arrow (Named (b, param2), ret))
;;

let arrow3 =
  fun param1 param2 param3 ret ->
  Type.arrow
    (Named (a, param1))
    (Arrow (Named (b, param2), Arrow (Named (c, param3), ret)))
;;

let app3 = fun x y z -> Term.app (Term.app2 x y z)
let app4 = fun x y z a -> Term.app (app3 x y z a)

let faulty_program =
  [ Statement.let'
      hummingbird
      (lambda6
         (Named (_A, Sort Sort.Type))
         (Named (_B, Sort Sort.Type))
         (Named (_C, Sort Sort.Type))
         (Named
            ( a
            , arrow3
                (Term (Var _B))
                (Term (Var _C))
                (Term (Var _B))
                (Term (Var _A)) ))
         (Named (b, Term (Var _B)))
         (Named (c, Term (Var _C)))
         (app3 (Var a) (Var b) (Var c) (Var b)))
  ; Statement.let'
      vireo
      (lambda6
         (Named (_A, Sort Sort.Type))
         (Named (_B, Sort Sort.Type))
         (Named (_C, Sort Sort.Type))
         (Named (a, Term (Var _A)))
         (Named (b, Term (Var _B)))
         (Named
            ( c
            , arrow2
                (Term (Var _A))
                (Term (Var _B))
                (Term (Var _C)) ))
         (Term.app2 (Var c) (Var a) (Var b)))
  ; Statement.let'
      delta
      (lambda3
         (Named (_A, Sort Sort.Type))
         (Named (_B, Sort Sort.Type))
         (Named (_C, Sort Sort.Type))
         (app4
            (Var hummingbird)
            (Var _A)
            (Var _B)
            (Var _C)
            (app3 (Var vireo) (Var _A) (Var _B) (Var _C))))
  ]
;;

let working_program =
  [ Statement.let'
      id
      (Term.lambda
         (Named (_T, Sort Sort.Type))
         (Term.lambda (Named (x, Term (Var _T))) (Var x)))
  ; Statement.print
      (Term.app2 (Term.var id) (Term.var _Nat) (Var _O))
  ]
;;

let program = working_program
