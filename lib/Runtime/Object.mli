open Custom
open Common

type t =
  | Constant of Primitive.t
  | Fun of Name.t * t
  | Late of Tail.Term.t

val show : (module Painter.TYPE) -> t -> string
