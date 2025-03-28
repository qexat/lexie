open Custom
open Common

type t =
  | Constant of Primitive.t
  | Fun of Name.t * t
  | Late of Tail.Term.t

let rec show =
  fun painter obj ->
  let module Painter = (val painter : Painter.TYPE) in
  match obj with
  | Constant prim -> Primitive.show painter prim
  | Fun (param, ret) ->
    Printf.sprintf
      "%s %s -> %s"
      (Painter.paint_keyword "fun")
      (Name.show painter param)
      (show painter ret)
  | Late _ -> Painter.paint_dim "<unevaluated>"
;;
