open Custom

type t =
  | Let of Name.t * Kind.t option * Term.t
  | Print of Term.t

val let' : Name.t -> ?annotation:Kind.t -> Term.t -> t
val print : Term.t -> t
val show : (module Painter.TYPE) -> t -> string
