open Common

type t = Lang.term

(* Constructors *)
let app = fun func arg -> Lang.App (func, arg)
let lambda = fun param ret -> Lang.Fun (param, ret)
let hole = Lang.Hole
let primitive = fun prim -> Lang.Primitive prim
let var = fun name -> Lang.Var name

(* Shorthands *)
let app2 = fun func arg1 arg2 -> app (app func arg1) arg2
let nat = fun n -> primitive (Primitive.nat n)
let unit = primitive Primitive.unit

(* Functions *)
let to_syntactic_kind = Lang.term_to_syntactic_kind
let show = Lang.show_term
