open Custom
open Common

(** A runtime value. *)
type t =
  | Constant of Primitive.t
  | Fun of Name.t * t
  | Late of Tail.Term.t

(** [show painter obj] produces a pretty-printable
    representation of [obj] using the [painter]. *)
val show : (module Painter.TYPE) -> t -> string
