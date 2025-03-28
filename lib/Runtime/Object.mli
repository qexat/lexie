open Custom
open Common

type t =
  | Constant of Primitive.t
  | Fun of Name.t * t
  | Late of Analysis.Term.t

val show : (module Painter.TYPE) -> t -> string
