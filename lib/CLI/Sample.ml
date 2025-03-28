open Custom
open Tail

let id = Name.of_string_exn "id"
let _T = Name.of_string_exn "T"
let _Nat = Name.of_string_exn "Nat"
let _Unit = Name.of_string_exn "Unit"
let x = Name.of_string_exn "x"
let _S = Name.of_string_exn "S"
let _O = Name.of_string_exn "O"

let program =
  [ Statement.let'
      id
      (Term.lambda (_T, Sort Sort.Type) (Term.lambda (x, Term (Var _T)) (Var x)))
  ; Statement.print (Term.app2 (Term.var id) (Term.var _Nat) (Var _O))
  ]
;;
