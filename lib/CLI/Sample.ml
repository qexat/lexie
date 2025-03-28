open Custom
open Analysis

let id = Name.of_string_exn "id"
let _T = Name.of_string_exn "T"
let x = Name.of_string_exn "x"

let program =
  [ Statement.let'
      id
      (Term.lambda (_T, Sort Sort.Type) (Term.lambda (x, Term (Var _T)) (Var x)))
  ; Statement.print (Term.var id)
  ]
;;
